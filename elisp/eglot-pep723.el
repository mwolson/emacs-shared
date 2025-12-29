;;; eglot-pep723.el --- PEP-723 support for Eglot -*- lexical-binding: t; -*-

(require 'cl-lib)

(defgroup eglot-pep723 nil
  "PEP-723 support for Eglot."
  :group 'eglot
  :prefix "eglot-pep723-")

;;;###autoload
(defcustom eglot-pep723-lsp-server 'ty
  "LSP server to use for Python files."
  :type '(choice (const :tag "ty" ty)
                 (const :tag "basedpyright" basedpyright))
  :group 'eglot-pep723)

;;;###autoload
(defcustom eglot-pep723-python-project-markers '("pyproject.toml" "requirements.txt")
  "Files that indicate a Python project root."
  :type '(repeat string)
  :group 'eglot-pep723)

(defun eglot-pep723-has-metadata-p (&optional file)
  "Return non-nil if FILE contains PEP-723 script metadata.
If FILE is nil, use the current buffer's file.
Scans first 2KB for `# /// script' ... `# ///' block."
  (let* ((file (or file (buffer-file-name)))
         (case-fold-search nil))
    (when file
      (save-match-data
        (with-temp-buffer
          (insert-file-contents-literally file nil 0 2048)
          (goto-char (point-min))
          (when (re-search-forward "^# /// script$" nil t)
            (re-search-forward "^# ///$" nil t)))))))

(defun eglot-pep723--uv-env-dir ()
  "Return the uv script environments directory.
Uses `uv cache dir` to get the cache location, then appends environments-v2/."
  (let ((cache-dir (string-trim (shell-command-to-string "uv cache dir"))))
    (unless (string-empty-p cache-dir)
      (file-name-as-directory
       (expand-file-name "environments-v2" cache-dir)))))

(defun eglot-pep723-get-python-path (script-path)
  "Get Python interpreter path for SCRIPT-PATH using uv.
Runs `uv python find --script SCRIPT-PATH'.
Displays a warning if the environment needs to be synced.
Returns the Python path, or nil if uv is not available."
  (if (not (executable-find "uv"))
      (progn
        (message "[eglot-pep723] uv not found")
        nil)
    (let* ((script-path (expand-file-name script-path))
           (env-dir (eglot-pep723--uv-env-dir))
           (default-directory (file-name-directory script-path))
           (output (shell-command-to-string
                    (format "uv python find --script %s"
                            (shell-quote-argument script-path))))
           (python-path (string-trim output)))
      (cond
       ((string-empty-p python-path)
        (message "[eglot-pep723] uv couldn't find Python for %s" script-path)
        nil)
       ((and env-dir (string-prefix-p env-dir python-path))
        python-path)
       (t
        (display-warning 'eglot-pep723
                         "Environment not synced. Run M-x eglot-pep723-sync-environment"
                         :warning)
        python-path)))))

(defun eglot-pep723--python-env-dir (python-path)
  "Return the environment directory for PYTHON-PATH.
Given a path like /path/to/env/bin/python3, return /path/to/env/."
  (when python-path
    (let ((bin-dir (file-name-directory python-path)))
      (when (string-match-p "/bin/?$" bin-dir)
        (file-name-directory (directory-file-name bin-dir))))))

(defvar eglot-server-programs)
(defvar eglot-workspace-configuration)

(defvar eglot-pep723--workspace-configs (make-hash-table :test 'equal)
  "Hash table mapping directory paths to workspace configurations.
Used by basedpyright to look up PEP-723 script configurations.")

(defun eglot-pep723--init-options ()
  "Return initializationOptions for ty LSP server.
For PEP-723 scripts, includes environment configuration.
Only used for ty; basedpyright uses workspace configuration instead."
  (when (eq eglot-pep723-lsp-server 'ty)
    (when-let* ((file (buffer-file-name))
                ((eglot-pep723-has-metadata-p file))
                (script-dir (file-name-directory file))
                (python-path (eglot-pep723-get-python-path file)))
      (let ((env-dir (or (eglot-pep723--python-env-dir python-path)
                         python-path)))
        `(:configuration
          (:environment
           (:python ,env-dir
            :root [,script-dir])))))))

(defun eglot-pep723--server-contact (_interactive)
  "Return the server contact spec for Python LSP.
Includes initializationOptions for ty with PEP-723 scripts."
  (let ((command (pcase eglot-pep723-lsp-server
                   ('ty '("ty" "server"))
                   ('basedpyright '("basedpyright-langserver" "--stdio"))))
        (init-options (eglot-pep723--init-options)))
    (if init-options
        `(,@command :initializationOptions ,init-options)
      command)))

(defun eglot-pep723--get-site-packages (python-path)
  "Get the site-packages directory for PYTHON-PATH."
  (when python-path
    (let* ((output (shell-command-to-string
                    (format "%s -c \"import site; print(site.getsitepackages()[0])\""
                            (shell-quote-argument python-path))))
           (site-packages (string-trim output)))
      (when (and (not (string-empty-p site-packages))
                 (file-directory-p site-packages))
        site-packages))))

(defvar eglot-pep723--original-workspace-configuration nil
  "Original value of `eglot-workspace-configuration' before we modified it.")

(defun eglot-pep723--workspace-config-fn (server)
  "Return workspace configuration, merging PEP-723 config if applicable.
Looks up configuration from `eglot-pep723--workspace-configs' based on
`default-directory', which Eglot sets before calling this function.
Falls back to original `eglot-workspace-configuration' for non-PEP-723 dirs."
  (let ((dir (file-name-as-directory (expand-file-name default-directory))))
    (or (gethash dir eglot-pep723--workspace-configs)
        ;; Fall back to original configuration
        (when eglot-pep723--original-workspace-configuration
          (if (functionp eglot-pep723--original-workspace-configuration)
              (funcall eglot-pep723--original-workspace-configuration server)
            eglot-pep723--original-workspace-configuration)))))

(defun eglot-pep723--setup-buffer ()
  "Configure buffer-local Eglot settings for a PEP-723 script.
Registers configuration in `eglot-pep723--workspace-configs' for basedpyright."
  (when (eq eglot-pep723-lsp-server 'basedpyright)
    (when-let* ((file (buffer-file-name))
                ((eglot-pep723-has-metadata-p file))
                (script-dir (file-name-as-directory
                             (expand-file-name (file-name-directory file))))
                (python-path (eglot-pep723-get-python-path file))
                (site-packages (eglot-pep723--get-site-packages python-path)))
      ;; Register config in global hash table keyed by directory
      (puthash script-dir
               `(:python (:pythonPath ,python-path)
                 :basedpyright.analysis (:extraPaths [,site-packages]))
               eglot-pep723--workspace-configs))))

(defun eglot-pep723--python-project-root-p (dir)
  "Return non-nil if DIR contains a Python project marker file."
  (seq-some (lambda (file)
              (file-exists-p (expand-file-name file dir)))
            eglot-pep723-python-project-markers))

(defun eglot-pep723--project-find (dir)
  "Project detection for Python files.
For PEP-723 scripts, returns (python-project . SCRIPT-DIR).
Otherwise, returns (python-project . ROOT) if DIR is inside a Python project."
  (cond
   ((and (memq major-mode '(python-mode python-ts-mode))
         (eglot-pep723-has-metadata-p))
    (cons 'python-project (file-name-directory (buffer-file-name))))
   ((locate-dominating-file dir #'eglot-pep723--python-project-root-p)
    (cons 'python-project (locate-dominating-file dir #'eglot-pep723--python-project-root-p)))))

(cl-defmethod project-root ((project (head python-project)))
  (cdr project))

;;;###autoload
(defun eglot-pep723-sync-environment ()
  "Sync the current PEP-723 script's environment, then restart Eglot.
Runs `uv sync --script' on the current file."
  (interactive)
  (let ((script-path (buffer-file-name)))
    (unless script-path
      (user-error "No file associated with buffer"))
    (unless (executable-find "uv")
      (user-error "uv not found"))
    (unless (eglot-pep723-has-metadata-p script-path)
      (user-error "Buffer does not contain PEP-723 metadata"))
    (let* ((default-directory (file-name-directory script-path))
           (status (call-process "uv" nil nil nil "sync" "--script"
                                 script-path)))
      (if (zerop status)
          (progn
            (message "Environment synced successfully")
            (when-let* ((server (eglot-current-server)))
              (eglot-reconnect server)))
        (message "Failed to sync environment (exit code: %s)" status)))))

;;;###autoload
(defun eglot-pep723-run-script ()
  "Run the current file as a PEP-723 script using `uv run'."
  (interactive)
  (let ((script-path (buffer-file-name)))
    (unless script-path
      (user-error "No file associated with buffer"))
    (unless (eglot-pep723-has-metadata-p script-path)
      (user-error "Buffer does not contain PEP-723 metadata"))
    (let ((default-directory (file-name-directory script-path)))
      (compile (format "uv run %s" (shell-quote-argument script-path))))))

;;;###autoload
(defun eglot-pep723-setup ()
  "Set up PEP-723 support for Python modes.
Adds hooks for project detection and Eglot configuration.
Configures `eglot-server-programs' based on `eglot-pep723-lsp-server'.
Call this after loading Eglot."
  (interactive)
  (add-to-list 'eglot-server-programs
               '((python-ts-mode python-mode) . eglot-pep723--server-contact))
  (add-hook 'project-find-functions #'eglot-pep723--project-find)
  ;; basedpyright needs workspace configuration via global function
  ;; Save original value so we can fall back to it for non-PEP-723 files
  (when (eq eglot-pep723-lsp-server 'basedpyright)
    (unless (eq (default-value 'eglot-workspace-configuration)
                #'eglot-pep723--workspace-config-fn)
      (setq eglot-pep723--original-workspace-configuration
            (default-value 'eglot-workspace-configuration))
      (setq-default eglot-workspace-configuration
                    #'eglot-pep723--workspace-config-fn)))
  (add-hook 'python-mode-hook #'eglot-pep723--setup-buffer)
  (add-hook 'python-ts-mode-hook #'eglot-pep723--setup-buffer))

(provide 'eglot-pep723)
;;; eglot-pep723.el ends here
