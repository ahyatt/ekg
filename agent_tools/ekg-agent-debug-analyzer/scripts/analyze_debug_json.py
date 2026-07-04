#!/usr/bin/env python3
"""Analyze an ekg-agent debug JSON export and print a Markdown report."""

from __future__ import annotations

import collections
import json
import sys
from pathlib import Path


READ_ONLY_TOOLS = {
    "get_notes_with_all_tags",
    "get_notes_with_any_tags",
    "get_note_by_id",
    "search_notes",
    "list_all_tags",
    "read_agents_md",
    "read_file",
    "list_buffers",
    "read_buffer",
    "list_org_items",
    "emacs_help_search",
    "emacs_help_symbol",
    "emacs_info_search",
    "emacs_info_node",
    "web_browse",
    "web_search",
}

STATE_CHANGING_TOOLS = {
    "append_to_note",
    "replace_note",
    "create_note",
    "run_elisp",
    "run_code_tool",
    "run_subagent",
    "edit_file",
    "write_file",
    "edit_buffer",
    "run_interactive_command",
    "run_command",
    "append_to_current_note",
    "replace_current_note",
    "add_org_item",
    "set_org_item_status",
}

FILE_CHANGE_TOOLS = {"write_file", "edit_file", "edit_buffer"}


def as_text(value: object) -> str:
    if value is None:
        return ""
    if isinstance(value, str):
        return value
    return json.dumps(value, sort_keys=True, ensure_ascii=False)


def char_count(value: object) -> int:
    if isinstance(value, str):
        return len(value)
    if isinstance(value, dict):
        count = 0
        for item in value.values():
            count += char_count(item)
        return count
    if isinstance(value, list):
        return sum(char_count(item) for item in value)
    return len(as_text(value))


def signature(name: str, args: object) -> str:
    return json.dumps([name, args], sort_keys=True, ensure_ascii=False)


def collect_interactions(data: dict) -> list[dict]:
    return data.get("prompt", {}).get("interactions", []) or []


def collect_tool_uses(interactions: list[dict]) -> list[dict]:
    uses: list[dict] = []
    for interaction in interactions:
        for use in interaction.get("tool_uses", []) or []:
            if isinstance(use, dict):
                item = dict(use)
                item["interaction_index"] = interaction.get("index")
                uses.append(item)
    return uses


def collect_tool_results(interactions: list[dict]) -> list[dict]:
    results: list[dict] = []
    for interaction in interactions:
        for result in interaction.get("tool_results", []) or []:
            if isinstance(result, dict):
                item = dict(result)
                item["interaction_index"] = interaction.get("index")
                results.append(item)
    return results


def finding(findings: list[tuple[str, str, str]], severity: str, title: str, detail: str) -> None:
    findings.append((severity, title, detail))


def analyze(data: dict) -> str:
    findings: list[tuple[str, str, str]] = []
    prompt = data.get("prompt", {})
    session = data.get("session", {})
    log_text = data.get("log", {}).get("text", "") or ""
    interactions = collect_interactions(data)
    tool_uses = collect_tool_uses(interactions)
    tool_results = collect_tool_results(interactions)
    history = data.get("tool_call_history", []) or []
    tools = prompt.get("tools", []) or []
    requirements = set(session.get("completion_requirements", []) or [])

    tool_use_names = [use.get("name", "") for use in tool_uses]
    history_names = [entry.get("name", "") for entry in history]
    successful_names = set(history_names)

    if not tool_uses:
        finding(
            findings,
            "high",
            "No tool calls were recorded in prompt interactions.",
            "The agent likely spent the session in plain text, failed before tool use, "
            "or the provider did not store tool-use turns in the prompt.",
        )

    plain_assistant = [
        item
        for item in interactions
        if item.get("role") == "assistant"
        and item.get("content_type") == "string"
        and as_text(item.get("content")).strip()
    ]
    if plain_assistant:
        finding(
            findings,
            "medium",
            f"{len(plain_assistant)} assistant text turn(s) did not call tools.",
            "These turns usually force an extra LLM round trip because ekg-agent "
            "must remind the model to call an end or action tool.",
        )

    error_results = [
        result
        for result in tool_results
        if as_text(result.get("result")).lstrip().startswith("Error:")
    ]
    if error_results:
        names = collections.Counter(result.get("tool_name", "unknown") for result in error_results)
        detail = ", ".join(f"{name}: {count}" for name, count in names.most_common())
        finding(findings, "high", f"{len(error_results)} tool error result(s).", detail)

    log_error_markers = [
        marker
        for marker in [
            "LLM error",
            "Error starting LLM request",
            "Error in callback",
            "Completion blocked",
            "Timeout reached",
            "force-cancelled",
        ]
        if marker in log_text
    ]
    if log_error_markers:
        finding(
            findings,
            "high",
            "Agent log contains failure/control-flow markers.",
            ", ".join(log_error_markers),
        )

    duplicate_counts = collections.Counter(
        signature(entry.get("name", ""), entry.get("args"))
        for entry in history
        if entry.get("name", "") in READ_ONLY_TOOLS
    )
    duplicates = [sig for sig, count in duplicate_counts.items() if count > 1]
    if duplicates:
        examples = []
        for sig in duplicates[:5]:
            name, args = json.loads(sig)
            examples.append(f"{name}({as_text(args)[:120]}) x{duplicate_counts[sig]}")
        finding(
            findings,
            "medium",
            f"{len(duplicates)} repeated successful tool signature(s).",
            "; ".join(examples),
        )

    consecutive = []
    previous = None
    for name in tool_use_names:
        if name and name == previous:
            consecutive.append(name)
        previous = name
    if consecutive:
        counts = collections.Counter(consecutive)
        detail = ", ".join(f"{name}: {count}" for name, count in counts.most_common())
        finding(findings, "medium", "Consecutive repeated tool choices.", detail)

    repeated_status = sum(
        1
        for a, b in zip(tool_use_names, tool_use_names[1:])
        if a == b == "summarize_state"
    )
    if repeated_status:
        finding(
            findings,
            "medium",
            "Repeated summarize_state calls.",
            "The status reminder or blocker may be encouraging status loops.",
        )

    reminder_count = sum(
        1
        for item in interactions
        if item.get("role") == "user"
        and "Before doing more work, call `summarize_state`" in as_text(item.get("content"))
    )
    if reminder_count > 1:
        finding(
            findings,
            "medium",
            f"{reminder_count} status reminder prompt(s) were injected.",
            "The agent may be taking too long between meaningful progress updates.",
        )

    long_results = [
        result
        for result in tool_results
        if (result.get("result_char_count") or char_count(result.get("result"))) > 10000
    ]
    if long_results:
        names = collections.Counter(result.get("tool_name", "unknown") for result in long_results)
        detail = ", ".join(f"{name}: {count}" for name, count in names.most_common())
        finding(
            findings,
            "medium",
            f"{len(long_results)} large tool result(s) over 10k characters.",
            detail,
        )

    total_prompt_chars = char_count(prompt.get("context")) + char_count(interactions)
    if total_prompt_chars > 80000:
        finding(
            findings,
            "medium",
            f"Prompt transcript is large ({total_prompt_chars:,} estimated characters).",
            "Consider tighter tool outputs, narrower reads, or summarizing older state.",
        )

    if "file-change" in requirements and not (successful_names & FILE_CHANGE_TOOLS):
        finding(
            findings,
            "high",
            "Required file change was not completed.",
            "completion_requirements contains file-change, but no edit/write tool "
            "is present in successful tool history.",
        )

    if "create-note" in requirements and "create_note" not in successful_names:
        finding(
            findings,
            "high",
            "Required note was not created.",
            "completion_requirements contains create-note, but create_note is absent "
            "from successful tool history.",
        )

    if "run-elisp" in requirements and "run_elisp" not in successful_names:
        finding(
            findings,
            "high",
            "Required run_elisp verification was not completed.",
            "completion_requirements contains run-elisp, but run_elisp is absent "
            "from successful tool history.",
        )

    if requirements and history and not (successful_names & STATE_CHANGING_TOOLS):
        finding(
            findings,
            "medium",
            "No successful state-changing tools were recorded.",
            "The session did work, but all successful tool history entries were "
            "read-only or informational.",
        )

    read_before_write = 0
    saw_write = False
    for name in history_names:
        if name in FILE_CHANGE_TOOLS:
            saw_write = True
        elif not saw_write and name in {"read_file", "read_buffer", "run_elisp"}:
            read_before_write += 1
    if "file-change" in requirements and read_before_write >= 4:
        finding(
            findings,
            "medium",
            "Long diagnosis/read loop before first edit.",
            f"{read_before_write} read/diagnostic calls occurred before any file edit.",
        )

    unused_tools = sorted(
        set(tool.get("name", "") for tool in tools if tool.get("name")) - set(tool_use_names)
    )
    if len(tools) >= 30 and len(unused_tools) / max(len(tools), 1) > 0.85:
        finding(
            findings,
            "low",
            "Large mostly-unused tool surface.",
            f"{len(tools)} tools were available; {len(unused_tools)} were unused.",
        )

    tool_counter = collections.Counter(tool_use_names)
    result_counter = collections.Counter(
        result.get("tool_name", "unknown") for result in tool_results
    )

    lines = [
        "# EKG Agent Debug Analysis",
        "",
        f"- Schema: {data.get('schema')} v{data.get('schema_version')}",
        f"- Generated: {data.get('generated_at')}",
        f"- Session buffer: {session.get('buffer_name')}",
        f"- Interactions: {len(interactions)}",
        f"- Tool uses: {len(tool_uses)}",
        f"- Tool results: {len(tool_results)}",
        f"- Successful tool history entries: {len(history)}",
        f"- Estimated prompt characters: {total_prompt_chars:,}",
        "",
        "## Findings",
        "",
    ]

    severity_order = {"high": 0, "medium": 1, "low": 2}
    findings.sort(key=lambda item: (severity_order.get(item[0], 99), item[1]))
    if findings:
        for severity, title, detail in findings:
            lines.append(f"- **{severity.upper()}** {title} {detail}")
    else:
        lines.append("- No obvious deterministic inefficiencies found.")

    lines.extend(["", "## Tool Use Counts", ""])
    if tool_counter:
        for name, count in tool_counter.most_common():
            errors = sum(1 for result in error_results if result.get("tool_name") == name)
            result_count = result_counter.get(name, 0)
            lines.append(f"- `{name}`: {count} use(s), {result_count} result(s), {errors} error(s)")
    else:
        lines.append("- No tool uses found.")

    lines.extend(["", "## Recommendation Prompts", ""])
    lines.append("- For high-severity failures, inspect the exact interaction indexes and log text before editing code.")
    lines.append("- Prefer improving existing tool descriptions, argument schemas, result limits, or guardrails before adding new tools.")
    lines.append("- Add a new tool only when repeated failures show the model lacks a single safe operation it cannot compose from existing tools.")
    lines.append("- Remove or hide tools when they are consistently unused or lure the model into the wrong workflow for this task type.")

    return "\n".join(lines) + "\n"


def main(argv: list[str]) -> int:
    if len(argv) != 2:
        print(f"Usage: {Path(argv[0]).name} DEBUG_EXPORT.json", file=sys.stderr)
        return 2
    path = Path(argv[1])
    with path.open("r", encoding="utf-8") as handle:
        data = json.load(handle)
    print(analyze(data), end="")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
