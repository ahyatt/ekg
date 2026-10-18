# EKG org-view creation experiments

## Recommendation

Use explicit, immediate title entry for the common cases:

- `c`: new sibling after the current task's complete subtree.
- `+`: new child appended to the selected task (already supported by EKG).
- `C` (Shift-c): new top-level task from anywhere in the tree.
- Keep visual placement as a separately named advanced command.

These are proposed bindings. This experiment does not change production bindings
or automatically load any prototype.

## What I tried

I implemented three opt-in Emacs Lisp prototypes and ran scripted trials against
real temporary SQLite databases and rendered EKG views in fresh batch Emacs 30.1.
Input answers and cancellation were supplied programmatically. This is an
interaction-design assessment with functional checks, not human usability
research, a visual GUI review, or a measured speed comparison.

| Approach | Interaction | Result and judgment |
| --- | --- | --- |
| Current flow (baseline) | `c`, position placeholder, Enter, title, Enter | Flexible, but the preview says “type task title” while letters still run movement commands. Defaults to first child. Logical slot order can make “next” move upward on screen. |
| Direct sibling / child | Choose sibling or child command, title, Enter | Best everyday default. The relationship is explicit before typing; no placement mode or extra confirmation. Siblings land after the complete subtree; children append after existing children. Arbitrary placement still needs another command. |
| Relationship chooser | Choose sibling/child in completion, title, Enter | Most explicit in a single entry point, but adds a recurring prompt for a choice that two commands can express directly. A reasonable alternative if one discoverable command matters more than frequent entry. |
| Title first, then visual placement | Title, Enter, position named preview, Enter | Improves the initial typing experience and shows the actual draft title while positioning. Still has two phases, inherits slot-order/depth behavior, and asks for placement even in routine cases. Useful advanced option. |

The two baseline issues were reproduced by executable assertions: `n` resolves
to a movement command despite the typing hint, and slot buffer positions are
not monotonically increasing in a nested fixture.

## Results

36/36 experiment checks passed. For each prototype, the checks cover sibling
creation after a subtree, appending a child, creating a nested sibling, blank
input, cancelling title entry, an empty view, and rejection in an archive view.
Additional checks cover cancelling title-first placement without saving and the
two baseline issues above. Assertions inspect stored task counts and rendered
hierarchy, rather than just checking that commands return successfully.

The direct-command and chooser prototypes now both offer explicit top-level
creation. A new root appends after the existing top-level tasks, regardless of
the selected task's depth. In a focused subtree view, successful creation opens
the global task view and selects the new root so it cannot silently disappear
outside the current scope. Twelve additional checks cover both approaches from
a grandchild, a focused subtree, and an empty tree, plus blank input,
cancellation, and archive rejection. Blank input and cancellation preserve point
and do not create a task. The top-level command is
`M-x ekg-experiment-top-level`; the chooser offers `top-level`.

The ordinary Eldev command is blocked because the installed package cache has
VUI 20261013.958 while EKG requires 20261017.1157. The isolated runner uses
available local dependency sources instead. The existing org suite passes 70/71
checks with prototypes unloaded; `ekg-org-test-view-reorder-after-warm-refresh`
fails with `args-out-of-range`. Full output is in `baseline.log`; experiment
output is in `trials.log`. These prototypes are
experimental; further checks of focused views, folded trees, concurrent refresh,
and actual keyboard interaction are needed before changing production defaults.

## Reproduce in this workspace

From `/workspace/ekg`:

```sh
PATH=/workspace/.cloud-setup/bin:$PATH emacs -Q --batch \
  -l experiments/new-item/run.el

PATH=/workspace/.cloud-setup/bin:$PATH EKG_EXPERIMENT_BASELINE=1 \
  emacs -Q --batch -l experiments/new-item/run.el
```

The runner's dependency discovery is specific to this workspace. The prototypes
themselves depend only on EKG and can be loaded into an already configured,
isolated EKG session:

```elisp
(load "/workspace/ekg/experiments/new-item/prototypes.el")
```

In an active org task view, try `M-x ekg-experiment-sibling`,
`ekg-experiment-child`, `ekg-experiment-top-level`, `ekg-experiment-chooser`, and
`ekg-experiment-title-first`. These commands create real tasks in that session's
database; use a disposable database. Loading the file adds preview/cleanup advice
for the title-first prototype, but does not change EKG's keymaps. Restart the
isolated session to discard the prototypes.
