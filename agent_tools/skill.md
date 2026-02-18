---
name: ekg
description: "Use this skill to supplement memory - both looking for and using memory to understand how to do things, and the state of tasks."
model: inherit
---

# ekg Skill — Emacs Knowledge Graph

`ekg` is an Emacs package ([source](https://github.com/ahyatt/ekg)) for taking
and retrieving notes, backed by a `sqlite` database. It should be used as a form
of agent memory, and it may also be used by users for their notes as well.

Here's how to use it from the command-line. Each script calls `emacsclient` so
the user must have run `M-x server-start` on their emacs, as well as loaded the
`ekg` package.

## Setup: Locating the Scripts

The command-line scripts live in the `agent_tools/` directory of the `ekg`
package. Since Emacs packages aren't normally on `$PATH`, resolve the directory
at the start of any session by asking Emacs where ekg is installed:

```sh
EKG_DIR=$(emacsclient --eval '(file-name-directory (locate-library "ekg"))' | tr -d '"')
```

Then invoke scripts with their full path, e.g. `"$EKG_DIR/agent_tools/ekg-read"`.

Alternatively, if you prefer, you can add the directory to your `$PATH`:

```sh
export PATH="$EKG_DIR/agent_tools:$PATH"
```

## ekg-add-note

Create a new note.

```sh
ekg-add-note --title "Title" --tag tag1 --tag tag2 --note "content" --mode org-mode
```

- `--title TEXT` — Note title. **Required for org tasks** (`org/task` tag),
  optional otherwise.
- `--tag TAG` — Tags (repeatable, at least one required).
- `--note TEXT` — Note content (required).
- `--mode MODE` — `org-mode` (default), `markdown-mode`, or `text-mode`.
- Returns the numeric note ID on success.
- The note is automatically tagged with `agent` and the current date (`date/YYYY-MM-DD`).
- Content is automatically wrapped in LLM output markers.

## ekg-read

Read notes by tag, ID, or search.

```sh
# By tags (AND logic)
ekg-read --tag tag1 --tag tag2 -n 10 -w 100

# By note ID
ekg-read 31123361376

# Semantic search (uses embeddings)
ekg-read --semantic-search "query string"

# Full-text search
ekg-read --search "query string"
```

- `-n NUM` — Max notes to return (default: 10).
- `-w NUM` — Max words per note (default: 100).
- Output is JSON: array of objects with `id`, `text`, `mode`, `tags`,
  `creation_time`, `modified_time`.

## ekg-update-tags

Add or remove tags on an existing note.

```sh
ekg-update-tags <note-id> --add tag1 --remove tag2
```

## ekg-tags

Search and list tags in ekg.

```sh
# List all tags
ekg-tags

# Tags matching a regex
ekg-tags --regex "org/state/.*"

# Tags with a given prefix
ekg-tags --prefix "project/"

# Tags containing a substring
ekg-tags --including "moltbot"

# Tags co-occurring with another tag (notes that have BOTH tags)
ekg-tags --co-tagged prompt

# Combine filters (AND logic)
ekg-tags --prefix "project/" --co-tagged moltbot
```

- `--regex PATTERN` — Filter tags matching a regex.
- `--prefix PREFIX` — Filter tags starting with prefix.
- `--including TEXT` — Filter tags containing substring.
- `--co-tagged TAG` — Find tags that appear on notes also tagged with TAG
  (repeatable, AND logic).

Output is a JSON array of tag strings.

## ekg-trash

Soft-delete a note (trash it). Running again on a trashed note permanently
deletes it.

```sh
ekg-trash <note-id>
```

## Tag Conventions

### Org Task Tags

With an `org/task` tag, notes will appear in the org-mode agenda (`/ekg:tasks.org`) once the
user sets it up by adding the file `/ekg:tasks.org` to the agenda.

| Tag | Purpose |
|---|---|
| `org/task` | Marks the note as an org task (required) |
| `org/state/todo` | Task is pending |
| `org/state/done` | Task is complete |
| `org/state/waiting` | Task is blocked |
| `org/archive` | Archived task (hidden from active agenda) |

**Org tasks require a `--title`** — this becomes the headline in the org agenda.

#### Creating an org task

```sh
ekg-add-note \
  --title "Implement feature X" \
  --tag "org/task" \
  --tag "org/state/todo" \
  --tag "moltbot" \
  --note "Description of the work to do." \
  --mode org-mode
```

### Marking a task done

```sh
ekg-update-tags <note-id> --add "org/state/done" --remove "org/state/todo"
```

### Archiving a completed task

```sh
ekg-update-tags <note-id> --add "org/archive"
```

### Prompt Co-Tags

Notes tagged with `prompt` plus other tags act as **injected instructions**.
This is used by the built-in ekg agent, but as an outside agent, you also need
to retrieve relevant prompts (if there are any) and use them before performing
any significant action.

Example: A note tagged `prompt` + `slack` should inject its instructions into any
agentic use of `slack`.

#### Creating a prompt note

```bash
ekg-add-note \
  --tag "prompt" \
  --tag "slack" \
  --tag "moltbot" \
  --note "Always reply in threads in Slack channels, never top-level." \
  --mode markdown-mode
```

To find all tags that have prompt co-tags:

```sh
ekg-tags --co-tagged prompt
```

To see all prompt notes:

```sh
ekg-read --tag "prompt" -n 20 -w 200
```

### General Tags

- `agent` — Automatically added to all notes created via `ekg-add-note`.
- `date/YYYY-MM-DD` — Automatically added with the current date.
- Project-specific tags (e.g., `project/ekg`, `slack`) for organization.

## Advanced: Direct Emacsclient

For operations not covered by the scripts, you can call elisp directly:

```bash
# Set a property on a note
emacsclient --eval '(progn
  (let ((note (ekg-get-note-with-id NOTE_ID)))
    (setf (ekg-note-properties note)
          (plist-put (ekg-note-properties note) :org/deadline TIMESTAMP))
    (ekg-save-note note)
    "done"))'

# Check a note's properties
emacsclient --eval '(ekg-note-properties (ekg-get-note-with-id NOTE_ID))'

# Get all org tasks as org-mode text
emacsclient --eval '(ekg-org-generate-org-content)'
```

Key properties for org tasks:

- `:titled/title` — List containing the title string (e.g., `(list "My Task")`)
- `:org/parent` — Parent task note ID (for subtasks)
- `:org/deadline` — Unix timestamp integer
- `:org/scheduled` — Unix timestamp integer

## Recommended Agent Workflow

1. **When starting work on something:** Create an `org/task` + `org/state/todo`
   note (add your own tag, e.g., `claude`) so it shows up in the org agenda.
2. **When finishing:** Update the tag to `org/state/done`.
3. **For reusable instructions:** Create `prompt` co-tagged notes so they get
   injected into future relevant tasks.
4. **For general knowledge:** Create regular notes with descriptive tags.
5. **Always search first:** Before creating notes, check if relevant ones
   already exist with `ekg-read`.  Before doing an action, check to see if
   any notes co-tagged with the `prompt` tag works.
