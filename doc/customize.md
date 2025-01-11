# Customizing Emacs

These can be customized using `(setq ...)`. For example:

```elisp
(setq my-frame-maximize-p t)
(setq my-emacs-path "~/emacs-shared/")

;; customizations must be made before this line
(load-file (concat my-emacs-path "init/shared-init.el"))
```

Most of the settings are optional, except for `my-emacs-path`.

## Display Settings

* `my-default-font`: This will be a font name and size, for example: `"Fira Code-17"`
* `my-default-emoji-font`: This will be a font name, for example: `"Noto Color Emoji"`
* `my-default-emoji-size`: This will be a font size, for example: `21` (it's recommended to be about 4 points higher than the normal font size on Linux, and same as the default font size for any other OS)
* `my-modus-theme`: Which [modus color theme](https://protesilaos.com/emacs/modus-themes-pictures) to apply; set this to `nil` if using `my-theme`
* `my-modus-theme-overrides`: Pallette overrides for `modus-themes-common-palette-overrides`
* `my-theme`: Which color theme to apply; set this to `nil` if using `my-modus-theme`
* `my-use-themes-p`: Whether to use a color theme
* `my-frame-maximize-p`: Whether to make the window fullscreen on startup
* `my-frame-pad-width`, `my-frame-pad-height`: The width and height left over for the macOS Dock
* `my-frame-height`: If the window is not fullscreen, it will be resized to have this many rows on startup
* `my-frame-width`: If the window is not fullscreen, it will be resized to have this many columns on startup

## Other Settings

* `my-default-directory`: After Emacs starts up, switch to this directory
* `my-emacs-path`: Location of your checkout of the `emacs-shared` repo
* `my-gptel-backend`: The `gptel` backend to use, as a symbol; examples are `'my--gptel-claude` and `'gptel--openai` . Make sure to set an appropriate value for `my-gptel-model` as well.
* `my-gptel-model`: The `gptel` model to use; if not specified, the first valid value for the backend will be used.
* `my-remap-cmd-key-p`: Whether to remap the <kbd>Command</kbd> key on macOS (or Windows key on Linux) to be mostly a clone of <kbd>Alt</kbd>, with some exceptions
* `my-settings-shared-p`: Whether to save customization settings into a personal file at `~/.emacs.d/settings.el` or use the ones that come with `emacs-shared` - a default is chosen based on whether that personal settings file is found
* `my-server-start-p`: Whether to start a server process that can be reused in multiple editing sessions
* `my-recent-files`: A list of files to open automatically in the background after starting Emacs
* `my-system-paths`: A list of directories to use when Emacs wants to find an executable program like `grep` - a large list of defaults is already provided based on OS
* `my-default-ripgrep-args`: A string containing arguments that are passed by default to ripgrep.

## Convenience Functions

* <kbd>M-x my-reset-font RET</kbd>: Call this to manually reset the font after you've changed `my-default-font`
* <kbd>M-x my-reset-frame-size RET</kbd>: Call this to manually reset the size of the window
* <kbd>M-x my-reset-theme RET</kbd>: Call this to manually reset the theme after you've changed `my-theme`
* <kbd>M-x my-fetch-url RET</kbd>: Fetch the contents of a URL into a new buffer
* `(my-defer-startup func)`: Call `func` shortly after Emacs has started - a good choice for moderately expensive tasks
* `(with-cpu-profiling ...code)`: Profile a block of code to see which function calls within it are taking the longest

---

[Back to README.md](../README.md#documentation)
