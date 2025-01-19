;;; company-minuet.el --- Company backend for minuet -*- lexical-binding: t; -*-

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
;; This package provides a company backend for minuet code completion.

;;; Code:

(require 'cl-seq)
(require 'company)
(require 'minuet)

(defgroup company-minuet nil
  "Company backend for minuet."
  :group 'company)

(defcustom company-minuet-idle-delay 1
  "Delay in seconds before minuet completion starts automatically."
  :type 'number
  :group 'company-minuet)

(defvar company-minuet--cached-candidates nil
  "Cache for completion candidates.")

(defvar company-minuet--last-prefix nil
  "Last prefix used for completion.")

(defun company-minuet--get-longest (it1 it2)
  (if (< (length it1) (length it2)) it2 it1))

(defun company-minuet--candidates-callback (candidates)
  "Store CANDIDATES in cache and trigger company completion."
  (setq candidates (list (cl-reduce #'company-minuet--get-longest candidates)))
  (setq company-minuet--cached-candidates candidates))

(defun company-minuet (command &optional arg &rest _ignored)
  "Company backend for minuet completion.
See `company-backends' for more info about COMMAND and ARG."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-minuet))
    (prefix (and (not (minuet-evil-not-insert-state-p))
                 (not (company-in-string-or-comment))
                 (or (company-grab-symbol) 'stop)))
    (candidates
     (when (and arg (not (equal arg company-minuet--last-prefix)))
       (setq company-minuet--last-prefix arg
             company-minuet--cached-candidates nil)
       ;; Request new candidates
       (let ((available-p-fn (intern (format "minuet--%s-available-p" minuet-provider)))
             (complete-fn (intern (format "minuet--%s-complete" minuet-provider)))
             (context (minuet--get-context)))
         (when (funcall available-p-fn)
           (funcall complete-fn context #'company-minuet--candidates-callback))))
     ;; Return cached candidates
     company-minuet--cached-candidates)
    (no-cache t)  ; Don't cache results between calls
    (ignore-case t)
    (require-match 'never)
    (doc-buffer nil)
    (meta nil)
    (annotation nil)
    (duplicates t)))

;;;###autoload
(defun company-minuet-setup ()
  "`company-mode' completion for minuet."
  (interactive)
  (make-local-variable 'company-minuet--cached-candidates)
  (make-local-variable 'company-minuet--last-prefix)
  (setq-local company-idle-delay company-minuet-idle-delay)
  (add-to-list 'company-backends 'company-minuet))

(provide 'company-minuet)
;;; company-minuet.el ends here
