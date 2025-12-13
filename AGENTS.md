# AGENTS.md

This repository is mostly an Emacs config, but a few parts have a reliable “dev
loop” for validating formatter integrations.

## Dev loop tools

### Quick Elisp sanity check

- Check parentheses balance after edits:
  - `emacs -Q --batch --eval '(progn (with-temp-buffer (insert-file-contents "init/shared-init.el") (check-parens)))'`

### One-off batch harnesses

When iterating on a small part of the config (a single function, hook, or
integration), prefer writing a tiny one-off `.el` file under `tmp/` and running
it via `emacs -Q --batch`. This keeps the feedback loop fast without requiring a
full interactive Emacs session.

- Example workflow:
  - Create `tmp/<topic>-test.el` that loads just what you need (either by
    copying the relevant forms or by selectively `load-file`-ing a small file).
  - Run it:
    - `emacs -Q --batch -l tmp/<topic>-test.el`
  - Print output with `princ` and exit non-zero with `(kill-emacs 1)` on
    failure.
