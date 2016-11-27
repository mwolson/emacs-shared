Customizing Emacs
-----------------

These can be customized using `(setq ...)`. For example:

```elisp
(setq my-frame-maximize-p t)
```

### Display

* `my-default-font`: This will be a font name and size, for example: `"Inconsolata-18"`
* `my-theme`: Color theme
* `my-use-themes-p`: Whether to use a color theme
* `my-frame-maximize-p`: Whether to make the window fullscreen on startup
* `my-frame-pad-width`, `my-frame-pad-height`: The width and height left over for the macOS Dock

### Other

* `my-default-directory`: After Emacs starts up, switch to this directory
* `my-emacs-path`: Location of your checkout of the `emacs-shared` repo
* `my-server-start-p`: Whether to start a server process that can be reused in multiple editing sessions

---

[Back to README.md](https://github.com/mwolson/emacs-shared#customizing-emacs)
