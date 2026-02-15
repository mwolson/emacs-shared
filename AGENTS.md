# AGENTS.md

This repository is mostly an Emacs config.

## Planning

Prefer to write plans in the `plans/` directory.

## Dev loop tools

Here are some strategies to obtain a reliable “dev loop” for validating
formatter integrations.

### Introspecting Elisp functions/variables from CLI

To check a function's arguments or documentation without starting interactive
Emacs:

- Get argument list (using `eglot` as an example):
  - `emacs -Q --batch --eval "(require 'eglot)" --eval "(princ (help-function-arglist 'eglot))"`
- Get function documentation (using `eglot` as an example):
  - `emacs -Q --batch --eval "(require 'eglot)" --eval "(princ (documentation 'eglot))"`
- Get variable documentation (using `eglot-server-programs` as an example):
  - `emacs -Q --batch --eval "(require 'eglot)" --eval "(princ (documentation-property 'eglot-server-programs 'variable-documentation))"`

### Quick Elisp sanity check

- Check parentheses balance after edits:
  - `emacs -Q --batch --eval '(progn (with-temp-buffer (insert-file-contents "init/shared-init.el") (check-parens)))'`

### Native compilation check

Run native compilation on all config files to catch warnings and errors:

- `./scripts/native-comp-all.sh`

This ensures all changes will compile cleanly. Look for warnings about:

- Obsolete macros/functions (e.g., `defadvice`, `when-let`, `incf`)
- Unused lexical variables/arguments
- References to free variables (usually fine in `with-eval-after-load` blocks)
- Unknown functions (usually fine when using autoloads or
  `with-eval-after-load`)

### Debugging init errors

The user's `~/.emacs.d/early-init.el` loads `init/early-shared-init.el`, and
`~/.emacs.d/init.el` loads `init/shared-init.el`.

To run the full user init headlessly:

- `emacs --batch -l ~/.emacs.d/early-init.el -l ~/.emacs.d/init.el`

Note: `--batch` alone does not load user init files; you must pass them
explicitly with `-l`. Some errors only occur with a live display (e.g., color
resolution, frame parameters) and cannot be reproduced in `--batch` mode; ask
the user to run `emacs --debug-init` and share the backtrace in those cases.

To reproduce display-dependent early-init issues from the CLI (e.g., when
`color-values` returns nil before the GUI frame is ready), launch GUI Emacs with
an isolated init directory and capture stderr:

- Create a minimal repro under `tmp/`, e.g. `tmp/repro/early-init.el`.
- Symlink packages if needed: `ln -s ~/.emacs.d/elpa tmp/repro/elpa`
- Launch with nohup (macOS example):
  - `EMACS_BIN="/opt/homebrew/Caskroom/emacs-plus-app/30.2-*/Emacs.app/Contents/MacOS/Emacs"`
  - `nohup $EMACS_BIN --init-directory=tmp/repro --debug-init > tmp/repro/nohup.out 2>&1 &`
- Check `tmp/repro/nohup.out` for the backtrace.
- To probe state interactively, add `(setq debug-on-error t)` to the
  `early-init.el` so that Emacs drops into the debugger in the GUI window
  instead of just logging to stderr.

After fixing issues in `init/*.el` files, always recompile native code:

- `./scripts/native-comp-all.sh`

Stale `.eln` files will keep running old code even after the `.el` source is
fixed. This is a common gotcha when the user reports "my fix didn't work."

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

## Gotchas

### JSON serialization

When working with JSON serialization issues (especially with LSP
configurations):

- **Test individual components**: Isolate problematic parts by testing JSON
  serialization of individual plists and values to identify the exact issue.

- **Use plists for JSON objects**: `json-serialize` can't handle lists
  containing other lists, but can handle plists:

  ```elisp
  ;; This fails:
  :args '("my-key-1" ("my-key-2" ("--format=unix" "--quiet" "%file")))

  ;; This works:
  :args '(:my-key-1 (:my-key-2 ["--format=unix" "--quiet" "%file"]))
  ```

- **Use vectors for JSON arrays**: `json-serialize` can't handle lists
  containing strings, but can handle vectors:

  ```elisp
  ;; This fails:
  :args '("--format=unix" "--quiet" "%file")

  ;; This works:
  :args ["--format=unix" "--quiet" "%file"]
  ```

- **Keyword vs string handling**: `json-serialize` expects keyword keys in
  plists, but values must be strings. When converting between formats, ensure
  proper type conversion:

  ```elisp
  ;; Convert keywords to strings for values:
  (substring (symbol-name key) 1)  ; Remove leading :
  ```

- **Incremental testing**: Build up the JSON structure piece by piece to isolate
  where serialization fails.

### Deprecated macros

- **`when-let` and `if-let`**: These are deprecated in favor of `when-let*` and
  `if-let*`. Always use the starred versions.

### Compile-time vs runtime evaluation

When silencing byte-compilation warnings about unknown functions or variables,
prefer `eval-when-compile` with `require` over `declare-function` or `defvar`.
This ensures we catch deprecations and API changes during compilation.

Key points about `eval-when-compile` vs `eval-and-compile`:

- Both forms run at compile time even when placed inside a function body
- `eval-when-compile`: runs at compile time only; the return value is baked into
  the compiled code
- `eval-and-compile`: runs at both compile time and runtime

Use cases:

- **Macro expansion**: Use `(eval-when-compile (require 'pkg))` before code that
  uses macros from `pkg`. The macro will expand at compile time. If the code
  also calls functions from `pkg` at runtime, add a separate `(require 'pkg)`.

- **Top-level compile-time checks**: Use `(eval-when-compile (require 'pkg))` at
  the top-level of a file to catch deprecation warnings for packages used in
  `with-eval-after-load` blocks. This also helps compile-angel pick up these
  references for automatic compilation.

- **Function-only calls**: If code only calls functions (no macros), a plain
  `(require 'pkg)` inside the function is sufficient - no compile-time require
  needed.

- **Package initialization**: Use `(eval-and-compile (package-initialize))` or
  similar when the side effects must happen at both compile time and runtime.

- **Settings file**: Load the settings file (which contains package
  customizations) at compile time before requiring packages:

  ```elisp
  (eval-when-compile
    (load (concat my-emacs-path "init/settings") nil t)
    (require 'some-package))
  ```

- **Functions not in ELPA packages**: Use `declare-function` for functions from
  packages that aren't installed via ELPA and have compile-time issues:
  ```elisp
  (declare-function my-fn "my-package")
  ```

### Customize type validation warnings

Rarely, some packages define defcustoms with types like `(repeat function)` but
use default values containing symbols for packages that do not match the type
due to not being loaded (e.g., `evil-goto-line`). When using `setopt` with such
variables, the customize system validates the value and warns if any symbols
aren't recognized as functions.

To avoid these spurious warnings, use `setq` instead of `setopt` for such
variables, with a comment explaining why:

```elisp
;; Use setq instead of setopt: pkg's defcustom type is (repeat function),
;; but its default includes symbols for packages not always loaded.
(setq pkg-some-variable modified-value)
```
