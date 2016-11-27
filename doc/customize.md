Customizing Emacs
=================

These can be customized using `(setq ...)`. For example:

```elisp
(setq my-frame-maximize-p t)
(setq my-emacs-path "~/emacs-shared/")

;; customizations must be made before this line
(load-file (concat my-emacs-path "init/shared-init.el"))
```

Most of the settings are optional, except for `my-emacs-path`.

## Display

* `my-default-font`: This will be a font name and size, for example: `"Inconsolata-18"`
* `my-theme`: Color theme
* `my-use-themes-p`: Whether to use a color theme
* `my-frame-maximize-p`: Whether to make the window fullscreen on startup
* `my-frame-pad-width`, `my-frame-pad-height`: The width and height left over for the macOS Dock
* `my-frame-height`: If the window is not fullscreen, it will be resized to have this many rows on startup
* `my-frame-width`: If the window is not fullscreen, it will be resized to have this many columns on startup

## Other

* `my-default-directory`: After Emacs starts up, switch to this directory
* `my-emacs-path`: Location of your checkout of the `emacs-shared` repo
* `my-settings-shared-p`: Whether to save customization settings into a personal file at `~/.emacs.d/settings.el` or use the ones that come with `emacs-shared` - a default is chosen based on whether that personal settings file is found
* `my-server-start-p`: Whether to start a server process that can be reused in multiple editing sessions
* `my-recent-files`: A list of files to open automatically in the background after starting Emacs
* `my-system-paths`: A list of directories to use when Emacs wants to find an executable program like `grep` - a large list of defaults is already provided based on OS

## Convenience functions

* <kbd>M-x my-reset-font RET</kbd>: Call this to manually reset the font after you've changed `my-default-font`
* <kbd>M-x my-reset-frame-size RET</kbd>: Call this to manually reset the size of the window
* <kbd>M-x my-reset-theme RET</kbd>: Call this to manually reset the theme after you've changed `my-theme`
* <kbd>M-x my-fetch-url RET</kbd>: Fetch the contents of a URL into a new buffer
* `(my-defer-startup func)`: Call `func` shortly after Emacs has started - a good choice for moderately expensive tasks
* `(with-cpu-profiling ...code)`: Profile a block of code to see which function calls within it are taking the longest

---

[Back to README.md](https://github.com/mwolson/emacs-shared#documentation)
