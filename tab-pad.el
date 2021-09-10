;;; tab-pad.el --- Pad Emacs 27's tabs to fill the frame/window -*- lexical-binding: t -*-

;; Author: Leonardo Schripsema
;; Created: 2020-05-28
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tabs
;; URL: https://github.com/leodag/tab-pad

;; Copyright (C) 2020 Leonardo Schripsema

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Currently only supports `tab-bar-mode', but supporting
;; `tab-line-mode' shouldn't be too much work.

;; Usage:

;; (tab-pad-bar-mode)

;; Idea taken from andreyorst's Making Emacs tabs look like in Atom,
;; with initial code taken from aorst/tab-bar-name-tab in his init.
;; https://github.com/andreyorst/dotfiles/blob/477525abb7ddf16437b40f3a8cbb02714790bd3c/.config/emacs/README.org#tabline

;; Some additional wizardry was needed to make it work with tab-bar,
;; mostly for dealing with the possibility of naming tabs. I needed a
;; way to save the "given name" (unpadded name) of a tab so it can be
;; repadded on posterior calls, but any attributes added to a tab were
;; deleted on tab switch. So I stored it as a text property on the
;; initial padding. Also, only changing the 'display attribute does
;; not trigger a redisplay, even after `force-mode-line-update', so we
;; also need to generate the appropriate amount of spaces with
;; `make-string' so that any changes to the text trigger a redisplay.

;;; Code:

(require 'cl-lib)


;;; Variables

(defgroup tab-pad nil
  "Pad Emacs 27's tabs to fill the frame/window"
  :prefix "tab-pad-")

(defcustom tab-pad-bar-min-width 20
  "Minimum width of a tab.")

(defcustom tab-pad-bar-max-width 300
  "Maximum width of a tab.")

(defcustom tab-pad-bar-fixed-overhead 1
  "Columns to be considered taken by fixed elements, such as starting
separator or history buttons.")

(defcustom tab-pad-bar-per-tab-overhead 1
  "Total extra columns that will be added to each tab, by elements
such as separators and close buttons.")


;;; Functions

;;;###autoload
(defun tab-pad-bar ()
  "Renames all tabs, padding them. Returns the new name of
the current tab."
  (let* ((tabs (or (frame-parameter nil 'tabs) (list (list 'current-tab))))
         (width (/ (frame-inner-width) (frame-char-width)))
         (unpadded-names (mapcar 'tab-pad--bar-get-name tabs))
         (padded-names (tab-pad--pad-names
                        unpadded-names
                        width
                        tab-pad-bar-min-width
                        tab-pad-bar-max-width
                        tab-pad-bar-fixed-overhead
                        tab-pad-bar-per-tab-overhead)))
    (cl-loop for tab in tabs
             for padded-name in padded-names
             do (setf (alist-get 'name tab) padded-name))
    (alist-get 'name (assq 'current-tab tabs))))

(defun tab-pad--bar-get-name (tab)
  "Gets the name of a tab, either from stored explicit name, current
buffer's name or stored buffer name."
  (cond ((alist-get 'explicit-name tab)
         (or (get-text-property 0 'given-name #1=(alist-get 'name tab))
             #1#))
        ((eq (car tab) 'current-tab)
         (buffer-name (window-buffer (minibuffer-selected-window))))
        (t
         (or (get-text-property 0 'given-name #1=(alist-get 'name tab))
             #1#))))

;; Despite using display to change amount of displayed spaces, we also need
;; to make actual spaces with make-string, since if there is no change in the
;; non-propertized text, the bar will not be redrawn on window resize
(defun tab-pad--pad-name (name width)
  "Pads NAME to WIDTH, with a minimum of 1 padding character on
each side.  If NAME is too wide, last character is turned into an
ellipsis."
  (let ((name-width (length name)))
    (if (> (+ name-width 2) width) ; plus two padding spaces minimum
        (concat (propertize " " 'given-name name)
                (truncate-string-to-width name (- width 2))
                "…") ; not as trucate ellipsis so we get " a…" instead of " aa"
      (let* ((padding-width (/ (- width name-width) 2.0))
             (left-padding (propertize (make-string (floor padding-width) ?\s)
                                       'display `(space :width ,(floor padding-width))
                                       'given-name name))
             (right-padding (propertize (make-string (ceiling padding-width) ?\s)
                                        'display `(space :width ,(ceiling padding-width)))))
        (concat left-padding name right-padding)))))

(defun tab-pad--pad-names (names width min-width max-width fixed-overhead per-tab-overhead &optional tab-amount)
  "Pads NAMES to a bar/line of width WIDTH, respecting MAX-WIDTH
and MIN-WIDTH, with column overheads FIXED-OVERHEAD and
PER-TAB-OVERHEAD.
If TAB-AMOUNT is specified, it will be used as the number of tabs
instead of the length of NAMES."
  (let* ((tab-amount (or tab-amount (length names)))
         (available-width (max (- width fixed-overhead) 1))
         (computed-width  (/ available-width tab-amount))
         (tab-width (- (cond ((> computed-width max-width)
                              max-width)
                             ((< computed-width min-width)
                              min-width)
                             (t
                              computed-width))
                       per-tab-overhead)))
    (mapcar (lambda (name) (tab-pad--pad-name name tab-width)) names)))

;;;###autoload
(define-minor-mode tab-pad-bar-mode
  "Pads tab-bar's names to fill the frame."
  :global t
  (cond
   (tab-pad-bar-mode
    (setq tab-bar-tab-name-function 'tab-pad-bar)
    (define-advice tab-bar-rename-tab (:after (&rest _r) tab-pad)
      (tab-pad-bar)))
   (t
    (setq tab-bar-tab-name-function 'tab-bar-tab-name-current)
    (advice-remove 'tab-bar-rename-tab 'tab-bar-rename-tab@tab-pad))))

(provide 'tab-pad)

;;; tab-pad.el ends here
