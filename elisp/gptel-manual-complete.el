;;; gptel-manual-complete.el --- Manual completion for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Michael Olson
;; Copyright (C) 2024  Karthik Chikmagalur

;; Author: Michael Olson <mwolson@gnu.org>
;; Keywords: hypermedia, convenience, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'gptel)
(require 'gptel-rewrite)

(defvar gptel-manual-complete-directive "Complete at end: ")

(defun gptel-manual-complete--mark-function-default (&optional steps)
  (let ((pt-min (point))
        (pt-mid (point))
        (pt-max (point)))
    (save-mark-and-excursion
      (ignore-errors
        (mark-defun steps)
        (setq pt-min (region-beginning)
              pt-max (region-end))))
    (save-mark-and-excursion
      (mark-paragraph steps)
      (when (<= (region-beginning) pt-min)
        (when (save-excursion
                (goto-char pt-mid)
                (beginning-of-line)
                (looking-at-p "[[:space:]]*$"))
          (forward-paragraph 1))
        (setq pt-min (region-beginning)
              pt-max (max pt-max (region-end)))))
    (set-mark pt-min)
    (goto-char pt-max)))

(defun gptel-manual-complete--mark-function-treesit (&optional steps)
  (treesit-end-of-defun)
  (let ((pt-max (point)))
    (treesit-beginning-of-defun)
    (setq steps (1- (- 0 (or steps 0))))
    (while (> steps 0)
      (treesit-beginning-of-defun)
      (cl-decf steps))
    (set-mark (point))
    (goto-char pt-max)))

(defun gptel-manual-complete--mark-function (&optional steps)
  "Put mark at end of this function, point at beginning.

If STEPS is negative, mark `- arg - 1` extra functions backward.
The behavior for when STEPS is positive is not currently well-defined."
  (interactive)
  (let ((pt-min (point))
        (pt-max nil))
    (when (null steps) (setq steps -1))
    (when (treesit-parser-list)
      (save-mark-and-excursion
        (gptel-manual-complete--mark-function-treesit steps)
        (setq pt-min (region-beginning)
              pt-max (region-end))))
    (gptel-manual-complete--mark-function-default steps)
    (when (< (region-beginning) pt-min)
      (setq pt-min (region-beginning)
            pt-max (region-end)))
    (goto-char pt-min)
    (while (and (looking-at-p "[[:space:]\r\n]")
                (< (point) pt-max))
      (forward-char))
    (setq pt-min (point))
    (goto-char (1- pt-max))
    (push-mark pt-min nil t)))

;;;###autoload (autoload 'gptel-manual-complete "gptel-manual-complete" nil t)
(defun gptel-manual-complete ()
  "Complete using an LLM.

Either the last function or the current region will be used for context."
  (interactive)
  (gptel-manual-complete--mark-function)
  (gptel-manual-complete-send))

(defun gptel-manual-complete-send ()
  "Complete using an LLM."
  (let* ((nosystem (gptel--model-capable-p 'nosystem))
         ;; Try to send context with system message
         (gptel-use-context
          (and gptel-use-context (if nosystem 'user 'system)))
         (prompt (list (or (get-char-property (point) 'gptel-rewrite)
                           (buffer-substring-no-properties (region-beginning) (region-end)))
                       "What is the required change?"
                       gptel-manual-complete-directive))
         (buffer (current-buffer)))
    (deactivate-mark)
    (when nosystem
      (setcar prompt (concat (car-safe (gptel--parse-directive
                                        gptel--rewrite-directive 'raw))
                             "\n\n" (car prompt))))
    (gptel-request prompt
      :dry-run nil
      :system gptel--rewrite-directive
      :stream gptel-stream
      :context
      (let ((ov (or (cdr-safe (get-char-property-and-overlay (point) 'gptel-rewrite))
                    (make-overlay (region-beginning) (region-end) nil t))))
        (overlay-put ov 'category 'gptel)
        (overlay-put ov 'evaporate t)
        (cons ov (generate-new-buffer "*gptel-manual-complete*")))
      :callback `(lambda (&rest args)
                   (apply #'gptel--rewrite-callback args)
                   (with-current-buffer ,buffer
                     (backward-char))))))

(provide 'gptel-manual-complete)
;;; gptel-manual-complete.el ends here

;; Local Variables:
;; outline-regexp: "^;; \\*+"
;; End:
