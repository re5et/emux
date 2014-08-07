;;; emux-term.el --- Emacs Lisp Behaviour-Driven Development framework

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


(defcustom emux-term-program
  "/bin/bash"
  "The program to use for emux term"
  :type  'string
  :group 'emux)

(defcustom emux-term-command-line-unbind-key-list
  '("C-x" "C-c" "C-h" "C-r" "C-s" "<ESC>")
  "Keys that should not fall through to term in term-char mode."
  :type 'list
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

(defcustom emux-mode-emux-term-bind-key-alist
  '(("M-r" . emux-term-reverse-search-history)
    ("M-p" . emux-term-previous-command)
    ("M-n" . emux-term-next-command)
    ("C-g" . emux-term-keyboard-quit)
    ("M-<" . emux-term-beginning-of-buffer)
    ("M->" . emux-term-end-of-buffer)
    ("M-v" . emux-term-scroll-down-command)
    ("C-v" . emux-term-scroll-up-command)
    ("C-p" . emux-term-previous-line)
    ("C-n" . emux-term-next-line)
    ("C-c r" . emux-term-rename)
    ("C-c -" . emux-term-vsplit)
    ("C-c |" . emux-term-hsplit)
    ("C-c K" . emux-term-destroy)
    ("C-c C-c" . emux-term-keyboard-quit))
  "Keys to bind in emux-mode. These bindings are for
emux-term commands you wish to execute from anywhere in an
emux terminal buffer."
  :type 'alist
  :group 'emux)

(emux-mode-map-bind emux-mode-emux-term-bind-key-alist)

(defun emux-term-create (&optional name command)
  "Create a new terminal with the buffer named NAME
and execute terminal command command"
  (interactive)
  (let* ((new-term (term emux-term-program))
         (name (or name "terminal")))
    (emux-mode)
    (emux-term-setup-keys)
    (emux-term-handle-close)
    (emux-term-rename name)
    (if command
        (emux-term-command command))
    new-term))

(defalias 'emux-term 'emux-term-create)

(defun emux-term-handle-close ()
  "Close current term buffer when `exit' from term buffer."
  (when (ignore-errors (get-buffer-process (current-buffer)))
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (proc change)
                            (when (string-match "\\(finished\\|exited\\)" change)
                              (kill-buffer (process-buffer proc)))))))

(defun emux-term-setup-keys ()
  (dolist (unbind-key emux-term-command-line-unbind-key-list)
    (cond
     ((stringp unbind-key) (setq unbind-key (read-kbd-macro unbind-key)))
     ((vectorp unbind-key) nil)
     (t (signal 'wrong-type-argument (list 'array unbind-key))))
    (define-key term-raw-map unbind-key nil))
  (dolist (element emux-term-command-line-bind-key-alist)
    (setq bind-key (car element))
    (setq bind-command (cdr element))
    (cond
     ((stringp bind-key) (setq bind-key (read-kbd-macro bind-key)))
     ((vectorp bind-key) nil)
     (t (signal 'wrong-type-argument (list 'array bind-key))))
    (define-key term-raw-map bind-key bind-command)))

(defun emux-term-rename (name)
  "Change current terminal name to NAME"
  (interactive "snew terminal name: ")
  (rename-buffer name t))

(defun emux-term-split-and-create (split-command &optional name command)
  "Split screen using SPLIT-COMMAND and creat a new multi-term,
passing the NAME and COMMAND arguments to emux-term-create"
  (funcall split-command)
  (other-window 1)
  (emux-term-create name command))

(defun emux-term-vsplit (&optional name command)
  "Split vertically and call emux-term-create with
the NAME and COMMAND arguments"
  (interactive)
  (emux-term-split-and-create 'split-window-vertically name command))

(defun emux-term-hsplit (&optional name command)
  "Split horizontally and call emux-term-create with
the NAME and COMMAND arguments"
  (interactive)
  (emux-term-split-and-create 'split-window-horizontally name command))

(defun emux-term-send-raw (string &optional buffer)
  "Send STRING to terminal in buffer BUFFER"
  (interactive "sSend string to terminal: ")
  (let ((buf (if buffer
                 buffer
               (current-buffer))))
    (term-send-raw-string string)
    (emux-term-focus-prompt)))

(defun emux-term-command (command &optional buffer)
  "Execute command COMMAND in terminal in buffer BUFFER"
  (interactive "scommand: ")
  (emux-term-send-raw (concat command "\C-m") buffer))

(defun emux-term-previous-command ()
  (interactive)
  (emux-term-focus-prompt)
  (term-send-up))

(defun emux-term-next-command ()
  (interactive)
  (emux-term-focus-prompt)
  (term-send-down))

(defun emux-term-backward-word ()
  "Move backward word in term mode."
  (interactive)
  (term-send-raw-string "\eb"))

(defun emux-term-forward-word ()
  "Move forward word in term mode."
  (interactive)
  (term-send-raw-string "\ef"))

(defun emux-term-forward-kill-word ()
  "Kill word in term mode."
  (interactive)
  (term-send-raw-string "\ed"))

(defun emux-term-backward-kill-word ()
  "Backward kill word in term mode."
  (interactive)
  (term-send-raw-string "\C-w"))

(defun emux-term-reverse-search-history ()
  (interactive)
  (emux-term-focus-prompt)
  (term-send-raw-string "\C-r"))

(defun emux-term-terminal-ring-yank ()
  (interactive)
  (emux-term-focus-prompt)
  (term-send-raw-string "\C-y"))

(defun emux-term-terminal-ring-yank-pop ()
  (interactive)
  (emux-term-focus-prompt)
  (term-send-raw-string "\ey"))

(defun emux-term-emacs-ring-yank ()
  "Yank the last item from the kill ring and send
to the current buffers terminal"
  (interactive)
  (emux-term-focus-prompt)
  (flet ((insert-for-yank (string) (term-send-raw-string string)))
    (yank)))

(defun emux-term-emacs-ring-yank-pop ()
  (interactive)
  (emux-term-focus-prompt)
  (dotimes (i (- (point) (mark t)))
    (term-send-backspace))
  (process-send-string
   (get-buffer-process (current-buffer))
   (current-kill 1)))

(defun emux-term-clear-previous-scrollback ()
  "Remove srollback older than previous screen and refocus prompt"
  (interactive)
  (delete-region (point-min) (- (point-max) (* (window-width) (window-height))))
  (emux-term-focus-prompt))

(defun emux-term-focus-prompt ()
  "Enter term-char-mode and put the cursor at the prompt"
  (interactive)
  (term-char-mode)
  (term-pager-eob)
  (recenter (- -1 (min (max 0 scroll-margin)
                       (truncate (/ (window-body-height) 4.0))))))

(defun emux-term-blur-prompt ()
  "Enter term-line-mode if in term-mode"
  (if (equal major-mode 'term-mode)
      (term-line-mode)))

(defun emux-term-destroy (&optional buffer)
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
         (process (get-buffer-process buffer)))
    (if process
      (and
       (set-process-sentinel process 'emux-term-cleanup)
       (kill-process process))
      (kill-buffer buffer))))

(defun emux-term-cleanup (process state)
  (kill-buffer (process-buffer process)))

(defun emux-term-beginning-of-buffer ()
  (interactive)
  (beginning-of-buffer)
  (emux-term-blur-prompt))

(defun emux-term-end-of-buffer ()
  (interactive)
  (emux-term-focus-prompt))

(defun emux-term-scroll-down-command ()
  (interactive)
  (scroll-down-command)
  (emux-term-blur-prompt))

(defun emux-term-scroll-up-command ()
  (interactive)
  (if (< (emux-lines-left) (window-height))
      (emux-term-focus-prompt)
    (scroll-up-command)))

(defun emux-term-previous-line ()
  (interactive)
  (previous-line)
  (emux-term-blur-prompt))

(defun emux-term-next-line ()
  (interactive)
  (let ((lines-left (emux-lines-left)))
    (cond
     ((equal lines-left 0) nil)
     ((equal lines-left 1) (emux-term-focus-prompt))
     ((> lines-left 1) (next-line)))))

(defun emux-term-keyboard-quit ()
  "Make sure that before keyboard quitting go back to term-char-mode"
  (interactive)
  (term-interrupt-subjob)
  (emux-term-focus-prompt))

(defun emux-lines-left ()
  (- (count-lines (point-min) (point-max))
     (line-number-at-pos)))

(defadvice isearch-backward (before emux-term-isearch-backward activate)
  "Go into term-line-mode when moving to beginning of buffer"
  (emux-term-blur-prompt))

(provide 'emux-term)
