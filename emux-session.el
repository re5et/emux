;;; emux-session.el --- Emacs Lisp Behaviour-Driven Development framework

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

(require 'emux-screen)

(defcustom emux-default-session
  "default"
  "Default session name.  If Non-nil, a session will
be created with the value as it's name."
  :type 'string)

(defcustom emux-mode-emux-session-bind-key-alist
  '(("C-c d" . emux-session-set-default-directory)
    ("C-c b" . emux-session-jump-to-global-buffer)
    ("C-c B" . emux-session-jump-to-session-buffer)
    ("C-c C-S-k" . emux-session-destroy))
  "keys to bind in emux-mode. These bindings are for
emux-session commands you wish to execute from anywhere in an
emux terminal buffer"
  :type 'alist
  :group 'emux)

(emux-mode-map-bind emux-mode-emux-session-bind-key-alist)

(defun emux-sessions ()
  (emux-get 'sessions))

(defun emux-session-create (&optional properties)
  (interactive)
  (let ((session (gensym "emux-session-")))
    (setplist session properties)
    (emux-set
     'sessions
     (cons
      session
      (emux-get 'sessions)))
    (emux-session-current session)
    (unless (emux-session-get :name)
      (emux-session-set :name (read-from-minibuffer "session name: ")))))

(defun emux-filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun emux-session-get (property &optional session)
  (let ((session (or session (emux-session-current))))
    (when (eq property :buffers) ; drop killed buffers
      (emux-session-set :buffers (emux-filter 'buffer-live-p (get session property))))
    (get session property)))

(defun emux-session-set (property value &optional session)
  (let ((session (or session (emux-session-current))))
    (put session property value)))

(defun emux-session-current (&optional session)
  (if session
      (emux-set 'current-session session))
  (emux-get 'current-session))

(defun emux-session-switch (&optional session)
  (interactive)
  (let ((switch-to-session
         (or
          session
          (emux-session-from-name
           (emux-completing-read
            "session: "
            (mapcar (lambda (symbol)
                      (get symbol :name))
                    (emux-get 'sessions)))))))
    (emux-session-current switch-to-session)))

(defun emux-session-from-name (name)
  (let ((found-session nil))
    (mapc (lambda (session)
            (if (equal name (emux-session-get :name session))
                (setf found-session session)))
          (emux-get 'sessions))
    found-session))

(defun emux-session-set-default-directory (path)
  (interactive "Dsession default directory: ")
  (emux-session-set :default-directory (directory-file-name path)))

(defun emux-session-destroy (&optional session)
  (interactive)
  (if (yes-or-no-p "Really destroy session along with all screens and terminals / processes?")
      (let ((session (or session (emux-session-current))))
        (mapc (lambda (screen)
                (emux-screen-destroy screen))
              (emux-session-get :screens session))
        (emux-set 'sessions (remove session (emux-get 'sessions)))
        (if (eq session (emux-session-current))
            (emux-session-current (car (emux-get 'sessions)))))))

(defun emux-session-buffer-name (&optional buffer session)
  "return emux session buffer name for BUFFER"
  (let ((session-name (emux-session-get :name session))
        (buffer-name (buffer-name buffer)))
    (if (eq 0 (string-match (concat session-name "/") buffer-name))
        buffer-name
      (format "%s/%s" session-name buffer-name))))

(defun emux-session-name-buffer (&optional form)
  "Apply emux session buffer name to BUFFER."
  (rename-buffer (emux-session-buffer-name (current-buffer)) t))

(defadvice emux-screen-create (after emux-session-screen-create activate)
  (emux-session-set
   :screens
   (cons (emux-screen-current) (emux-session-get :screens))))

(defadvice emux-screen-switch (around emux-session-screen-switch activate)
  (flet ((emux-screens () (emux-session-get :screens)))
    ad-do-it))

(defadvice emux-term-create (around emux-session-default-directory activate)
  (let ((emux-default-directory (emux-session-get :default-directory)))
    (if (and emux-default-directory (string-match "/\\(.*\\):\\(.*\\)" emux-default-directory))
        (let ((ssh-scheme (match-string 1 emux-default-directory))
              (directory (match-string 2 emux-default-directory))
              (terminal-name (ad-get-arg 0))
              (original-command (ad-get-arg 1)))
          (message (concat "name: " terminal-name))
          (message (concat "command: " original-command))
          (let ((command nil))
            ad-do-it)
          (emux-term-command (concat "ssh " ssh-scheme))
          (emux-term-command (concat "cd " directory))
          (emux-term-command (concat original-command)))
      (let ((default-directory
              (if emux-default-directory
                  (concat emux-default-directory "/")
                default-directory)))
        ad-do-it))))

(defadvice emux-term-create (after emux-store-session-buffer activate)
  (emux-session-set
   :buffers
   (cons
    (current-buffer)
    (emux-session-get :buffers))))

(defadvice emux-term-rename (after emux-session-terminal-rename activate)
  (emux-session-name-buffer))

(defadvice emux-screen-current (around emux-session-current-screen activate)
  (emux-session-set :current-screen ad-do-it))

(defun emux-session-buffers (&optional session)
  (let ((session (or session (emux-session-current))))
    (emux-flatten (mapcar (lambda (screen)
                            (emux-screen-get :buffers screen))
                          (emux-session-get :screens session)))))

(defun emux-session-global-buffers ()
  (emux-flatten
   (mapcar
    (lambda (session)
      (emux-session-buffers session))
    (emux-get 'sessions))))

(defun emux-session-jump-to-global-buffer ()
  (interactive)
  (let ((buffer (emux-completing-read
                 "jump to global buffer: "
                 (mapcar 'buffer-name (emux-session-global-buffers)))))
    (catch 'break
      (mapc
       (lambda (session)
         (mapc
          (lambda (screen)
            (if (member (get-buffer buffer) (emux-screen-get :buffers screen))
                (progn
                  (emux-session-switch session)
                  (emux-screen-switch screen)
                  (pop-to-buffer buffer)
                  (message
                   (format
                    "switched to session %s and screen %s"
                    (emux-session-get :name session)
                    (emux-screen-get :name screen)))
                  (throw 'break nil))))
          (emux-session-get :screens session)))
       (emux-sessions)))))

(defun emux-session-jump-to-session-buffer ()
  (interactive)
  (let ((buffer
         (emux-completing-read
          "jump to session buffer: "
          (mapcar
           (lambda (buffer) (buffer-name buffer))
           (emux-session-buffers (emux-session-current))))))
    (catch 'break
      (mapc (lambda (screen)
              (if (member (get-buffer buffer) (emux-screen-get :buffers screen))
                  (progn
                    (emux-screen-switch screen)
                    (pop-to-buffer buffer)
                    (throw 'break nil))))
            (emux-session-get :screens)))))

(defmacro emux-session-define-template (name &rest body)
  `(let ((session-templates (emux-get 'session-templates)))
     (emux-set
      'session-templates
      (push '(,name  (progn ,@body)) session-templates))
     (print (emux-get 'session-templates))))

(defun emux-session-load-template ()
  (interactive)
  (let ((template
         (emux-completing-read
          "load emux session template: "
          (mapcar
           (lambda (item) (symbol-name (car item)))
           (emux-get 'session-templates)))))
    (when (cond
           ((not (emux-session-from-name template)) t)
           (t (and
               (yes-or-no-p
                (format "session %s already exists, reload?" template))
               (emux-session-destroy))))
      (emux-session-create `(:name ,template))
      (eval (cadr (assoc (intern template) (emux-get 'session-templates))))
      (message "%s template loaded" template))))

(if emux-default-session
    (and
     (emux-session-create
      `(:name ,emux-default-session))
     (emux-session-set-default-directory "~/")
     (emux-screen-create
      `(:name ,emux-default-session
              :no-terminal t))))

(provide 'emux-session)
