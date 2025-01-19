;;; minuet-capf.el --- Completion at point for minuet -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Michael Olson
;; Copyright (C) 2025  Milan Glacier

;; Author: Michael Olson <mwolson@gnu.org>
;; Maintainer: Milan Glacier <dev@milanglacier.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29") (company "0.9.13") (minuet "0.2"))

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;; This package provides the ability to use minuet for AI completions
;; using `completion-at-point'.

;;; Code:

(require 'minuet)
(require 'cl-seq)

(defvar-local minuet-capf--last-prefix nil
  "Last prefix used for completion.")

(defun minuet-capf--get-longest (it1 it2)
  (if (< (length it1) (length it2)) it2 it1))

(defun minuet-capf--candidates-callback (candidates)
  (list (cl-reduce #'minuet-capf--get-longest candidates)))

(defun minuet-completion-at-point ()
  "Completion at point function for minuet."
  (when (and (not (minuet-evil-not-insert-state-p))
             (not (nth 8 (syntax-ppss)))) ; not in string/comment
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (or (car bounds) (point)))
           (end (or (cdr bounds) (point)))
           (prefix (buffer-substring-no-properties start end))
           (candidates nil))
      (when (not (equal prefix minuet-capf--last-prefix))
        (setq minuet-capf--last-prefix prefix
              minuet-capf--cached-candidates nil)
        (let ((available-p-fn (intern (format "minuet--%s-available-p" minuet-provider)))
              (complete-fn (intern (format "minuet--%s-complete" minuet-provider)))
              (context (minuet--get-context)))
          (when (funcall available-p-fn)
            (setq candidates (funcall complete-fn context #'minuet-capf--candidates-callback)))))
      (list start
            end
            minuet-capf--cached-candidates
            :exclusive 'no))))

;;;###autoload
(defun minuet-capf-setup ()
  "Setup completion-at-point for minuet."
  (interactive)
  (add-hook 'completion-at-point-functions #'minuet-completion-at-point nil t))

(provide 'minuet-capf)
