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

(require 'emux-base)

(defun emux-terminal-create (&optional name command)
  "Create a new terminal with the buffer named NAME
and execute terminal command command"
  (interactive)
  (let* ((new-term (term "/bin/zsh"))
         (name (or name "terminal")))
    (emux-mode)
    (emux-terminal-setup-keys)
    (emux-terminal-handle-close)
    (emux-terminal-rename name)
    (if command
        (emux-terminal-command command))
    new-term))

(defun emux-terminal-handle-close ()
  "Close current term buffer when `exit' from term buffer."
  (when (ignore-errors (get-buffer-process (current-buffer)))
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (proc change)
                            (when (string-match "\\(finished\\|exited\\)" change)
                              (kill-buffer (process-buffer proc)))))))

(defun emux-terminal-setup-keys ()
  (dolist (unbind-key emux-terminal-command-line-unbind-key-list)
    (cond
     ((stringp unbind-key) (setq unbind-key (read-kbd-macro unbind-key)))
     ((vectorp unbind-key) nil)
     (t (signal 'wrong-type-argument (list 'array unbind-key))))
    (define-key term-raw-map unbind-key nil))
  (dolist (element emux-terminal-command-line-bind-key-alist)
    (setq bind-key (car element))
    (setq bind-command (cdr element))
    (cond
     ((stringp bind-key) (setq bind-key (read-kbd-macro bind-key)))
     ((vectorp bind-key) nil)
     (t (signal 'wrong-type-argument (list 'array bind-key))))
    (define-key term-raw-map bind-key bind-command)))

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

(defun emux-terminal-previous-command ()
  (interactive)
  (emux-terminal-focus-prompt)
  (term-send-prior))

(defun emux-terminal-next-command ()
  (interactive)
  (emux-terminal-focus-prompt)
  (term-send-next))

(defun emux-terminal-backward-word ()
  "Move backward word in term mode."
  (interactive)
  (term-send-raw-string "\eb"))

(defun emux-terminal-forward-word ()
  "Move forward word in term mode."
  (interactive)
  (term-send-raw-string "\ef"))

(defun emux-terminal-forward-kill-word ()
  "Kill word in term mode."
  (interactive)
  (term-send-raw-string "\ed"))

(defun emux-terminal-backward-kill-word ()
  "Backward kill word in term mode."
  (interactive)
  (term-send-raw-string "\C-w"))

(defun emux-terminal-reverse-search-history ()
  (interactive)
  (emux-terminal-focus-prompt)
  (term-send-raw-string "\C-r"))

(defun emux-terminal-terminal-ring-yank ()
  (interactive)
  (emux-terminal-focus-prompt)
  (term-send-raw-string "\C-y"))

(defun emux-terminal-terminal-ring-yank-pop ()
  (interactive)
  (emux-terminal-focus-prompt)
  (term-send-raw-string "\ey"))

(defun emux-terminal-emacs-ring-yank ()
  "Yank the last item from the kill ring and send
to the current buffers terminal"
  (interactive)
  (emux-terminal-focus-prompt)
  (flet ((insert-for-yank (string) (term-send-raw-string string)))
    (yank)))

(defun emux-terminal-emacs-ring-yank-pop ()
  (interactive)
  (emux-terminal-focus-prompt)
  (dotimes (i (- (point) (mark t)))
    (term-send-backspace))
  (process-send-string
   (get-buffer-process (current-buffer))
   (current-kill 1)))

(defun emux-terminal-clear-screen ()
  "Remove all output from current buffer
and enter term-char-mode"
  (interactive)
  (mark-whole-buffer)
  (delete-region (point-min) (point-max))
  (if (equal major-mode 'term-mode)
      (term-char-mode)))

(defun emux-terminal-focus-prompt ()
  "Enter term-char-mode and put the cursor at the prompt"
  (interactive)
  (term-char-mode)
  (term-pager-eob)
  (recenter (- -1 (min (max 0 scroll-margin)
                       (truncate (/ (window-body-height) 4.0))))))

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

(defun emux-beginning-of-buffer ()
  (interactive)
  (beginning-of-buffer)
  (emux-terminal-blur-prompt))

(defun emux-end-of-buffer ()
  (interactive)
  (emux-terminal-focus-prompt))

(defun emux-scroll-down-command ()
  (interactive)
  (scroll-down-command)
  (emux-terminal-blur-prompt))

(defun emux-scroll-up-command ()
  (interactive)
  (if (< (emux-lines-left) (window-height))
      (emux-terminal-focus-prompt)
    (scroll-up-command)))

(defun emux-previous-line ()
  (interactive)
  (previous-line)
  (emux-terminal-blur-prompt))

(defun emux-next-line ()
  (interactive)
  (let ((lines-left (emux-lines-left)))
    (cond
     ((equal lines-left 0) nil)
     ((equal lines-left 1) (emux-terminal-focus-prompt))
     ((> lines-left 1) (next-line)))))

(defun emux-lines-left ()
  (- (count-lines (point-min) (point-max))
     (line-number-at-pos)))

(defadvice isearch-backward (before emux-terminal-isearch-backward activate)
  "Go into term-line-mode when moving to beginning of buffer"
  (emux-terminal-blur-prompt))

(defun emux-keyboard-quit ()
  "Make sure that before keyboard quitting go back to term-char-mode"
  (interactive)
  (term-interrupt-subjob)
  (emux-terminal-focus-prompt))

(provide 'emux-terminal)
