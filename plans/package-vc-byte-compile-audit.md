# package-vc byte-compile audit

Packages updated to compile a narrower target in `init/`:

- Single-file packages now using `:main-file`: `compile-angel`, `ligature`,
  `maxframe`, `inheritenv`, `mise`, `cond-let`, `llama`, `transient`,
  `websocket`, `atomic-chrome`, `js-comint`, `dumb-jump`, `markdown-mode`,
  `web-mode`, `marginalia`, `nerd-icons-completion`, `flx`,
  `browse-kill-ring`, `add-node-modules-path`, `clojure-ts-mode`, `diminish`,
  `edit-indirect`, `el-mock`, `fish-mode`, `graphql-ts-mode`, `jtsx`,
  `kdl-mode`, `kotlin-ts-mode`, `lua-mode`, `nix-ts-mode`,
  `prisma-ts-mode`, `rainbow-delimiters`, `reformatter`, `swift-ts-mode`,
  `tmux-mode`, `minions`, `toc-org`, `eglot-python-preset`,
  `eglot-typescript-preset`.

- Subdir packages now using `:lisp-dir` or `:lisp-dir` plus `:main-file`:
  `magit`, `magit-section`, `with-editor`, `cider`, `basic-mode`.

Packages intentionally left broader for now:

- `hydra`: MELPA excludes `lv.el`, but the package also ships `hydra-ox.el`
  and `hydra-examples.el`. `package-vc` cannot express MELPA's exclusion-only
  recipe without also dropping potentially useful extensions.
- `vertico` and `corfu`: MELPA includes extension files explicitly. Restricting
  to `:main-file` would skip compiling shipped extensions the config may load.
- `embark`: MELPA includes `embark.el` plus `embark-org.el`; current config
  already splits `embark-consult`, but `embark-org` is still part of the main
  recipe.
- `consult`, `cape`, `diff-hl`, `git-modes`, `gptel`, `parseclj`,
  `color-theme-sanityinc-tomorrow`, `archive-rpm`: these ship multiple real
  libraries, not just tests or docs.
- `apheleia` and `nerd-icons`: MELPA's file selection includes extra runtime
  assets or helper directories that do not map cleanly to `:main-file`.
- No MELPA recipe available locally for `plz`, `popon`, `spinner`, `svg-lib`,
  `pulsar`, `flymake-stylelint`, `svelte-ts-mode`, `vue-ts-mode`,
  `corfu-terminal`, `kind-icon`, `majutsu`.

If we want a third pass, the main remaining improvement would be custom advice
to honor a package-local `:compile-files` style extension in `package-vc`, since
that would cover MELPA-style recipes more faithfully than `:main-file`.
