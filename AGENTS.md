# Agents Guide for EKG

## Build/Test/Lint Commands
- **Test all**: `eldev test`
- **Test single file**: `eldev test ekg-test.el` or `eldev test ekg-llm-test.el`
- **Lint**: `eldev lint`
- **Compile**: `eldev compile` (check for warnings — docstring width, unused vars, etc.)
- **Run with debug**: `eldev -p -dtT test` (shows full output with debug traces)

## Code Style Guidelines
- **File format**: Emacs Lisp with `lexical-binding: t` in first line
- **Headers**: Include standard copyright, GPL license, commentary section
- **Imports**: Use `require` for dependencies, `declare-function` for external functions
- **Naming**: Kebab-case with `ekg-` prefix for public functions, `ekg--` for private
- **Functions**: Use `cl-defun` with keyword args when appropriate, docstrings required
- **Variables**: Use `defcustom` for user options with `:type` and `:group`
- **Constants**: Use `defconst` with `ekg-` prefix
- **Tests**: Use `ekg-deftest` macro (sets up temp DB), place in `*-test.el` files
- **Errors**: Use `error` for user-facing messages, `warn` for recoverable issues
- **Formatting**: Standard Emacs Lisp indentation, max ~80 chars per line
- **Comments**: Use `;;; Code:` section headers, `;;` for inline comments

## What is EKG

EKG is a notetaking application for Emacs, which stores everything in SQLite in
a triples database (a dependency of this package).

All functionality is documented in the file `//doc/ekg.org`, which should be
consulted before changing anything.

## Data Model

- Notes are stored in SQLite via the `triples` library (subject-predicate-object format)
- Each note has: ID, text content, mode, tags, timestamps, and optional resources (URLs/files)
- Tags can have prefixes (e.g., "date/2024-01-01", "doc/filename", "person/username"), the prefixes can be meaningful on their own.
- Notes support different major modes (org-mode, markdown-mode, text-mode)

## Key Concepts

1. **Tag-centric design**: Notes are primarily organized by tags rather than titles
2. **Metadata editing**: Special metadata section at top of notes for editing tags and properties
3. **Resource notes**: Notes can be attached to files or URLs
4. **Templates**: Note creation can use templates based on tags
5. **Inline commands**: Support for transclusion and other dynamic content
6. **Semantic search**: Optional embedding-based search via LLM providers

All functionality is explained in depth in `doc/ekg.org.`.  Read this to
understand how functionality is supposed to work before modifying it.

To modify documentation, just modify `doc/ekg.org`.  This is then exported to
texinfo to generate the other formats in this directory.

## Testing Architecture

- Tests use `ekg-deftest` macro which sets up temporary databases
- Test utilities in `ekg-test-utils.el` provide database setup and teardown
- Tests cover note lifecycle, tag management, embedding functionality, and integrations

## Testing with a Fresh Emacs

Use a fresh Emacs when verifying agent behavior, which generally is too disruptive to the user.

- To find the executable path for the Emacs build you are currently using,
  evaluate this in that Emacs:
  `(expand-file-name invocation-name invocation-directory)`
- From a shell, the equivalent for the `emacs` on `PATH` is:
  `emacs --batch --eval '(princ (expand-file-name invocation-name invocation-directory))'`
- Prefer a named daemon so `emacsclient` does not connect to a normal user
  session:
  ```sh
  EMACS="/path/from/the/previous/step"
  SERVER="ekg-fresh-test"
  INIT_DIR="$(mktemp -d "${TMPDIR:-/tmp}/ekg-emacs.XXXXXX")"
  HOME="$INIT_DIR" "$EMACS" -Q --bg-daemon="$SERVER"
  ```
- Use an `emacsclient` from the same Emacs installation when possible:
  ```sh
  CLIENT="$(dirname "$EMACS")/emacsclient"
  [ -x "$CLIENT" ] || CLIENT="$(dirname "$EMACS")/bin/emacsclient"
  [ -x "$CLIENT" ] || CLIENT="$(command -v emacsclient)"
  "$CLIENT" -s "$SERVER" --eval '(emacs-version)'
  ```
- If the fresh daemon needs EKG and its Eldev-installed dependencies, first run
  an Eldev command with the same Emacs executable so dependencies are available:
  `EMACS="$EMACS" eldev test ekg-agent-test.el`
  Then load the repository and the matching `.eldev/<emacs-version>/packages/*`
  directories into the daemon before `(require 'ekg-agent)`.
- Stop the isolated daemon when finished:
  `"$CLIENT" -s "$SERVER" --eval '(kill-emacs)'`

## Database Schema

Uses `triples` library for RDF-like storage in SQLite:
- Notes stored as triples with various predicates (text, tags, mode, etc.)
- Supports full-text search via `triples-fts`
- Backup and upgrade functionality via `triples-backups` and `triples-upgrade`

## Agent Architecture (`ekg-agent.el`)

- The agent loop is in `ekg-agent--iterate`, which calls `llm-chat-async` and
  recurses on each tool result until an end tool is called.
- `llm-chat-async` returns a request handle (curl process from plz) that can be
  cancelled via `llm-cancel-request`. This is stored in `ekg-agent--current-request`.
- Tool functions that spawn subprocesses should be `:async t` and track their
  processes (or `futur` objects) in `ekg-agent--tool-processes` for force-cancellation support.
- `ekg-agent--set-stopped` acts as a once-only guard — it returns non-nil only on
  the first running→stopped transition, preventing double-firing of status callbacks.
- The request chain: `ekg-agent--iterate` → `llm-chat-async` → `llm-request-plz-async`
  → `plz-media-type-request` → `plz` (curl process named "plz-request-curl").
- `futur-process-call` (used by `run_elisp`) creates a subprocess via `futur`;
  callbacks are bounced to the main thread via `run-at-time`.

## CLI Tools (`agent_tools/`)

- Shell scripts in `agent_tools/` provide CLI access to ekg for external agents.
- Scripts call `emacsclient` so require a running Emacs with `server-start`.
- `agent_tools/skill.md` documents the scripts for use as an agent skill.
- To locate scripts programmatically, query Emacs:
  `emacsclient --eval '(file-name-directory (locate-library "ekg"))'`

## Development Notes

- Package requires Emacs 28.1+ and depends on `triples` and `llm` packages
- Uses Eldev for development workflow (testing, linting, dependency management)
- All code follows standard Emacs Lisp conventions with `lexical-binding: t`
- All code in this package is designed for distribution to other users, so
  nothing can depend on a specific emacs setup.
