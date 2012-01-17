;;; emux-terminal.el --- Emacs Lisp Behaviour-Driven Development framework

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

(require 'multi-term)

(defun emux-terminal-create (&optional name command)
  "Create a new terminal with the buffer named NAME
and execute terminal command command"
  (interactive)
  (let* ((new-term (multi-term))
         (name (or name "terminal")))
    (emux-terminal-rename name)
    (if command
        (emux-terminal-command command))
    new-term))

(defun emux-terminal-rename (name)
  "Change current terminal name to NAME"
  (interactive "snew terminal name: ")
  (rename-buffer name t))

(defun emux-terminal-split-and-create (split-command &optional name command)
  "Split screen using SPLIT-COMMAND and creat a new multi-term,
passing the NAME and COMMAND arguments to emux-terminal-create"
  (funcall split-command)
  (other-window 1)
  (emux-terminal-create name command))

(defun emux-terminal-vsplit (&optional name command)
  "Split vertically and call emux-terminal-create with
the NAME and COMMAND arguments"
  (interactive)
  (emux-terminal-split-and-create 'split-window-vertically name command))

(defun emux-terminal-hsplit (&optional name command)
  "Split horizontally and call emux-terminal-create with
the NAME and COMMAND arguments"
  (interactive)
  (emux-terminal-split-and-create 'split-window-horizontally name command))

(defun emux-terminal-send-raw (string &optional buffer)
  "Send STRING to terminal in buffer BUFFER"
  (interactive "sSend string to terminal: ")
  (let ((buf (if buffer
                 buffer
               (current-buffer))))
    (term-send-raw-string string)))

(defun emux-terminal-command (command &optional buffer)
  "Execute command COMMAND in terminal in buffer BUFFER"
  (interactive "scommand: ")
  (emux-terminal-send-raw (concat command "\C-m") buffer))

(defun emux-terminal-yank ()
  "Yank the last item from the kill ring and send
to the current buffers terminal"
  (interactive)
  (emux-terminal-focus-prompt)
  (emux-terminal-send-raw (current-kill 0 t)))

(defun emux-terminal-clear-screen ()
  "Remove all output from current buffer
and enter term-char-mode"
  (interactive)
  (mark-whole-buffer)
  (delete-region (point-min) (point-max))
  (term-char-mode))

(defun emux-terminal-focus-prompt ()
  (interactive)
  "Enter term-char-mode and put the cursor at the prompt"
  (if (equal major-mode 'term-mode)
      (progn
        (term-char-mode)
        (term-pager-eob))))

(defun emux-terminal-blur-prompt ()
  "Enter term-line-mode if in term-mode"
  (if (equal major-mode 'term-mode)
      (term-line-mode)))

(defun emux-terminal-destroy (&optional buffer)
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
         (process (get-buffer-process buffer)))
    (set-process-buffer process nil)
    (kill-process process)
    (kill-buffer buffer)))

(defadvice scroll-down (before emux-terminal-scroll-down activate)
  "Jump into line mode on scroll-down"
  (emux-terminal-blur-prompt))

(defadvice previous-line (before emux-terminal-previous-line activate)
  "Jump into line mode on previous-line"
  (emux-terminal-blur-prompt))

(defadvice beginning-of-buffer (before emux-terminal-beginning-of-buffer activate)
  "Go into term-line-mode when moving to beginning of buffer"
  (emux-terminal-blur-prompt))

(defadvice isearch-backward (before emux-terminal-isearch-backward activate)
  "Go into term-line-mode when moving to beginning of buffer"
  (emux-terminal-blur-prompt))

(defadvice term-previous-input (before emux-terminal-previous-input activate)
  "Go to term-char-mode when attemting to use previous command"
  (emux-terminal-focus-prompt))

;; (defadvice end-of-buffer (after emux-terminal-end-of-buffer activate)
;;   "refocus prompt on end of buffer"
;;   (emux-terminal-focus-prompt))

(defadvice term-interrupt-subjob (before emux-terminal-interrupt-subjob activate)
  "Make sure that before keyboard quitting go back to term-char-mode"
  (emux-terminal-focus-prompt))

(defadvice keyboard-quit (before emux-terminal-keyboard-quit activate)
  "Make sure that before keyboard quitting go back to term-char-mode"
  (emux-terminal-focus-prompt))

(defadvice he-substitute-string (around emux-hippie-expand activate)
  (term-send-backward-kill-word)
  (emux-terminal-send-raw (ad-get-arg 0)))

(provide 'emux-terminal)
