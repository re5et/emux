;;; emux-base.el --- Emacs Lisp Behaviour-Driven Development framework

;; Copyright (C) 2011 atom smith

;; Author: atom smith
;; URL: http://trickeries.com/emux
;; Created: 19 Jan 2011
;; Version: 0.1
;; Keywords: terminal multiplexer

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs; see the file COPYING, or type `C-h C-c'. If not,
;; write to the Free Software Foundation at this address:

;; Free Software Foundation
;; 51 Franklin Street, Fifth Floor
;; Boston, MA 02110-1301
;; USA

;;; Commentary:

;;

(defgroup emux nil
  "emux terminal multiplexer"
  :group 'emux)

(defcustom emux-completing-read-command
  'completing-read
  "The completing read command emux-completing-read
will use."
  :group 'emux)

(define-minor-mode emux-mode
  "emux Minor mode. Enhances working with terminals

\\{emux-mode-map}"
  nil
  " emux"
  :group 'emux
  :keymap (make-sparse-keymap))

(defun emux-set (property value)
  "Internal emux use, set emux properties"
  (put 'emux property value))

(defun emux-get (property)
  "Internal emux use, get emux properties"
  (get 'emux property))

(defun emux-completing-read (prompt collection)
  "Internal emux use, completing read used by emux.
Uses custom variable emux-completing-read-command. PROMPT
will be used as the text shown on the minibuffer completion,
COLLECTION is the list of possible completions."
  (funcall emux-completing-read-command prompt collection))

(defun emux-mode-map-bind (alist)
  (mapc
   (lambda (pair)
     (message (car pair))
     (define-key emux-mode-map (read-kbd-macro (car pair)) (cdr pair)))
   alist))

(defun emux-flatten (x)
  "Internal eumx use, generic list flattener."
  (cond ((null x) nil)
        ((listp x) (append (emux-flatten (car x)) (emux-flatten (cdr x))))
        (t (list x))))

(provide 'emux-base)
