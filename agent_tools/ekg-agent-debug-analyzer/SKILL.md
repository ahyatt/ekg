---
name: ekg-agent-debug-analyzer
description: Analyze EKG agent debug JSON exports produced by `ekg-agent-export-debug-json`; use when investigating slow, looping, failed, or inefficient ekg-agent sessions and when deciding whether to improve prompts, tool descriptions, tool schemas, result truncation, guardrails, or add/remove ekg-agent tools.
---

# EKG Agent Debug Analyzer

Use this skill to turn an `ekg-agent-export-debug-json` file into concrete
agent improvements. The goal is not just to summarize the session; identify
where the agent wasted turns, failed, chose the wrong tool, received bad tool
output, or lacked the right tool affordance, then make the smallest effective
change.

## Workflow

1. Run the bundled analyzer:

   ```sh
   python3 path/to/ekg-agent-debug-analyzer/scripts/analyze_debug_json.py /tmp/ekg-agent-debug-....json
   ```

2. Read the generated findings, then inspect the JSON directly for the cited
   interaction indexes, tool names, log markers, and result sizes.

3. Classify each issue:
   - Prompt issue: the instructions or completion blockers nudge the agent into
     the wrong behavior.
   - Tool description/schema issue: the correct tool exists, but the model does
     not know when or how to call it.
   - Tool output issue: a tool returns too much text, too little structure, or
     errors that are hard to recover from.
   - Missing tool issue: the agent repeatedly tries to compose an operation that
     should be one safe, explicit tool.
   - Bad tool issue: a tool is consistently unused, dangerous for the workflow,
     or attracts wrong calls.

4. Prefer changes in this order:
   - Tighten `ekg-agent-instructions-intro` or completion blocker messages.
   - Improve existing `make-llm-tool` descriptions and argument metadata.
   - Add result limits, paging, summaries, or structured return data to noisy
     tools.
   - Add a new tool only when the transcript shows repeated failed composition.
   - Remove or hide tools only when evidence shows they hurt the task class.

5. When editing EKG itself, update focused tests in `ekg-agent-test.el`. Run:

   ```sh
   eldev test ekg-agent-test.el
   eldev compile
   ```

## JSON Fields

Important fields in the export:

- `prompt.interactions`: the conversation in prompt order. Each item has
  `role`, `content_type`, `content`, `tool_uses`, `tool_results`, and
  `multi_turn_plist`.
- `prompt.tools`: the tool surface available to the model, including names,
  descriptions, args, and async flags.
- `tool_call_history`: successful tools recorded by EKG in chronological order.
  Errors are usually visible in `prompt.interactions[*].tool_results` and
  `log.text`.
- `session.completion_requirements`: inferred requirements that can block an
  attempted end tool.
- `configuration`: relevant limits and retry/status settings.
- `log.text`: user-visible log lines, including timeout, cancellation, LLM
  error, and completion-blocked markers.

## Report Format

When reporting back, lead with findings ordered by severity. For each finding,
include the evidence path, such as `prompt.interactions[7].tool_results[0]`,
the tool name, and the observed consequence. Then list proposed code or tool
changes and the tests that cover them.
