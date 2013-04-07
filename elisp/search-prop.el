;;; search-prop.el --- search forward and backward for properties

;; Copyright (C) 2007 Michael Olson

;; Author: Michael Olson (mwolson AT gnu DOT org)
;; Date: Sun 21-Jan-2007
;; Version: 1.3
;; URL: http://mwolson.org/static/dist/elisp/search-prop.el

;; This file not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; The routines in this file are used to quickly search through text
;; properties in the current buffer.  It is even possible to look for
;; a property with a particular value.

;; The first application of these methods was for finding the next (or
;; previous) instance of a URL.  This is accomplished by searching for
;; text that has already been fontified, in order to identify that of
;; the fontified face of that URL.

;; `search-property' does most of the work.  It is a generalized
;; search routine that takes the direction of the search, whether the
;; search is permitted to cycle to the other end of the buffer, the
;; property to search for, and (optionally) the particular value that
;; the property should have.

;; The search will wrap around to the other end of the buffer when the
;; CYCLE argument is passed to `search-property'.

;; This library also provides the functions `search-property-forward'
;; and `search-property-backward', which should be reminiscent of the
;; Emacs functions `search-forward' and `search-backward'.

;; The author has a copyright assignment on file for Emacs, in case
;; its authors wish to incorporate this code into Emacs proper at some
;; point.

;;; History:

;; 1.3:
;;
;; Change the interface for `search-property-forwrard' and
;; `search-property-backward' to be more like that of `search-forward'
;; and `search-backward'.  The COUNT argument from the latter two
;; functions has not been implemented, because I do not think it
;; belongs at this particular level of abstraction.
;;
;; Remove the `search-property-cycle-p' variable since it was
;; ill-conceived.

;; 1.2:
;;
;; Fix bug with consecutive text with the same property but different
;; values.
;;
;; Add (interactive) spec to `search-property-forwrard' and
;; `search-property-backward'.  Thanks to Andreas Roehler and
;; Christoph Conrad for the suggestion.

;; 1.1:
;;
;; Initial release.

;;; Code:

(defun search-property (direction cycle prop &optional val)
  "Search according to DIRECTION in current buffer for property PROP.
The position of the found property is returned, or nil if none
was found.

If DIRECTION is 'forward or t, search forward.  Otherwise, search
backward, and place the point at the beginning of the region that
satisfies the search.

If CYCLE is non-nil, permit the search to wrap around to the
other end of the buffer.

If VAL is given, search for an instance of PROP whose value is
VAL."
  (if (or (eq direction 'forward)
          (eq direction t))
      (setq direction t)
    (setq direction nil))
  (let ((pos (point))
        (next (point))
        move wrap-point end-point point-focus)
    (if direction
        (setq move #'next-single-property-change
              wrap-point (point-min)
              end-point (point-max)
              point-focus #'identity)
      (setq move #'previous-single-property-change
            wrap-point (point-max)
            end-point (point-min)
            point-focus #'1-))
    (let ((prop-at-point (if direction
                             (get-text-property (point) prop)
                           (if (= (point) (point-min))
                               nil
                             (get-text-property (1- (point)) prop)))))
      ;; move past property if at point
      (when (and (not (eq (point) end-point))
                 (if val
                     (eq val prop-at-point)
                   prop-at-point))
        (setq next (or (funcall move (point) prop)
                       end-point))
        (unless val
          ;; skip past all non-nil instances of prop
          (while (and (not (eq next end-point))
                      (get-text-property (funcall point-focus next)
                                         prop))
            (setq next (or (funcall move next prop)
                           end-point))))))
    ;; move to next match
    (let ((cycled nil))
      (while (and (if (eq next end-point)
                      (if (not cycle)
                          nil
                        (setq next wrap-point)
                        (setq cycled t))
                    t)
                  (if (and (eq next wrap-point)
                           (if val
                               (eq val (get-text-property
                                        (funcall point-focus next)
                                        prop))
                             (get-text-property (funcall point-focus next)
                                                prop)))
                      ;; found a match immediately after wrapping
                      (prog1 nil
                        (setq pos next))
                    (or (setq next (funcall move next prop))
                        (unless (or cycled (not cycle))
                          (setq cycled t)
                          (setq next (funcall move wrap-point prop)))))
                  (if val
                      (let ((prop-at-point (get-text-property
                                            (funcall point-focus next)
                                            prop)))
                        (if (eq val prop-at-point)
                            (prog1 nil (setq pos next))
                          t))
                    (prog1 nil (setq pos next))))))
    (unless (eq (point) pos)
      (unless direction
        ;; if searching backwards, put the point at the beginning of
        ;; the region with the property, not the end
        (setq pos (1- pos))
        (unless (eq pos (point-min))
          (let ((prop-at-point (get-text-property (funcall point-focus pos)
                                                  prop)))
            (when (if val
                      (eq val prop-at-point)
                    prop-at-point)
              (setq pos (or (funcall move pos prop)
                             end-point))))))
      (goto-char pos)
      pos)))

(defun search-property-forward (prop &optional val bound noerror)
  "Search forward in the current buffer for property PROP.
Set point to the beginning of the occurrence found, and return
point.

If VAL is given, search for an instance of PROP whose value is
VAL.

An optional third argument bounds the search; it is either a
buffer position, the symbol 'cycle, or nil.
  If a buffer position, the property must not begin after that
position.
  If 'cycle, permit the search to cycle to the beginning of
the buffer.
  If nil, the bound is (point-max).

An optional fourth argument specifies the behavior when no
matching property is found.
  If nil, throw an error.
  If t, just return nil instead of throwing an error.
  If not nil and not t move to the limit of the search and return
nil."
  (interactive
   (list (intern (read-string "Property: "))
         (let ((val (read-string "Value: ")))
           (if (string= val "") nil (intern val)))))
  (let ((cyclep (eq bound 'cycle))
        result)
    (if (and bound (not cyclep))
        (save-restriction
          (when (> (point) bound)
            (error "Invalid search bound (wrong side of point)"))
          (narrow-to-region (point) bound)
          (setq result (search-property 'forward cyclep prop val)))
      (setq result (search-property 'forward cyclep prop val)))
    (cond (result result)
          ((eq noerror t) nil)
          ((null noerror)
           (error "Search failed"))
          ((and bound (not cyclep))
           (goto-char bound)
           nil)
          (t (goto-char (point-max))
             nil))))

(defun search-property-backward (prop &optional val bound noerror)
  "Search backward in the current buffer for property PROP.
Set point to the beginning of the occurrence found, and return
point.

If VAL is given, search for an instance of PROP whose value is
VAL.

An optional third argument bounds the search; it is either a
buffer position, the symbol 'cycle, or nil.
  If a buffer position, the property must not end before that
position.
  If 'cycle, permit the search to cycle to the end of the buffer.
  If nil, the bound is (point-min).

An optional fourth argument specifies the behavior when no
matching property is found.
  If nil, throw an error.
  If t, just return nil instead of throwing an error.
  If not nil and not t move to the limit of the search and return
nil."
  (interactive
   (list (intern (read-string "Property: "))
         (let ((val (read-string "Value: ")))
           (if (string= val "") nil (intern val)))))
  (let ((cyclep (eq bound 'cycle))
        result)
    (if (and bound (not cyclep))
        (save-restriction
          (when (< (point) bound)
            (error "Invalid search bound (wrong side of point)"))
          (narrow-to-region bound (point))
          (setq result (search-property 'backward cyclep prop val)))
      (setq result (search-property 'backward cyclep prop val)))
    (cond (result result)
          ((eq noerror t) nil)
          ((null noerror)
           (error "Search failed"))
          ((and bound (not cyclep))
           (goto-char bound)
           nil)
          (t (goto-char (point-min))
             nil))))

(provide 'search-prop)

;; search-prop.el ends here
