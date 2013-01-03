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

(defcustom emux-term-command-line-unbind-key-list
  '("C-x" "C-c" "C-h" "C-r" "C-s" "<ESC>")
  "Keys to unbind from the command line in emux-mode."
  :type 'list
  :group 'emux)

(defcustom emux-mode-bind-key-alist
  '(("C-x r" . emux-term-rename)
    ("C-x K" . emux-term-destroy)
    ("C-x C" . emux-screen-create)
    ("C-x R" . emux-screen-rename)
    ("C-x s" . emux-screen-switch)
    ("C-x M-s" . emux-jump-to-screen)
    ("C-x C-S-k" . emux-session-destroy)
    ("C-x B" . emux-jump-to-buffer)
    ("C-x -" . emux-term-vsplit)
    ("C-x |" . emux-term-hsplit)
    ("M-r" . emux-term-reverse-search-history)
    ("M-<" . emux-beginning-of-buffer)
    ("M->" . emux-end-of-buffer)
    ("M-p" . emux-term-previous-command)
    ("M-n" . emux-term-next-command)
    ("M-v" . emux-scroll-down-command)
    ("C-v" . emux-scroll-up-command)
    ("C-n" . emux-next-line)
    ("C-p" . emux-previous-line)
    ("C-c C-c" . emux-keyboard-quit)
    ("C-g" . emux-keyboard-quit))
  "Keys to bind in emux-mode. These bindings are for commands
you wish to execute from anywhere in an emux terminal buffer"
  :type 'alist
  :group 'emux)

(defcustom emux-term-command-line-bind-key-alist
  '(("M-f" . emux-term-forward-word)
    ("M-b" . emux-term-backward-word)
    ("M-d" . emux-term-forward-kill-word)
    ("M-DEL" . emux-term-backward-kill-word)
    ("C-y" . emux-term-terminal-ring-yank)
    ("M-y" . emux-term-terminal-ring-yank-pop)
    ("C-S-y" . emux-term-emacs-ring-yank)
    ("M-Y" . emux-term-emacs-ring-yank-pop))
  "Keys to bind in term char-mode. These bindings are for
commands you wish to execute from the command line in an
emux terminal buffer"
  :type 'alist
  :group 'emux)

(defcustom emux-completing-read-command
  'completing-read
  "The completing read command emux-completing-read
will use."
  :group 'emux)

(define-minor-mode emux-mode
  "Minor mode to enhance working with terminals"
  nil
  " emux"
  :group 'emux
  :keymap (let ((emux-mode-map (make-sparse-keymap)))
            (mapc (lambda (pair)
                    (define-key emux-mode-map (read-kbd-macro (car pair)) (cdr pair)))
                  emux-mode-bind-key-alist)
            emux-mode-map))

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

(defun emux-flatten (x)
  "Internal eumx use, generic list flattener."
  (cond ((null x) nil)
        ((listp x) (append (emux-flatten (car x)) (emux-flatten (cdr x))))
        (t (list x))))

(provide 'emux-base)
