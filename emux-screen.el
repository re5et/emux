;;; emux-screen.el --- Emacs Lisp Behaviour-Driven Development framework

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

(require 'cl)
(require 'emux-base)
(require 'emux-term)

(defcustom emux-mode-emux-screen-bind-key-alist
  '(("C-c C" . emux-screen-create)
    ("C-c R" . emux-screen-rename)
    ("C-c s" . emux-screen-switch)
    ("C-c C-k" . emux-screen-destroy))
  "Keys to bind in emux-mode. These bindings are for
emux-screen commands you wish to execute from anywhere in an
emux terminal buffer."
  :type 'alist
  :group 'emux)

(emux-mode-map-bind emux-mode-emux-screen-bind-key-alist)

(defun emux-screens ()
  "Internal emux use, accessor that returns current screens"
  (emux-get 'screens))

(defun emux-screen-get (property &optional screen)
  "Internal emux use, screen property getter.  Gets the property
PROPERTY from the screen SCREEN. If SCREEN is not passed in
the current screen will be used."
  (let ((screen (or screen (emux-screen-current))))
    (get screen property)))

(defun emux-screen-set (property value &optional screen)
  "Internal emux use, screen property setter.  Sets the property
PROPERTY to VALUE fom the screen SCREEN. If SCREEN is not
passed in the current screen will be used."
  (let ((screen (or screen (emux-screen-current))))
    (put screen property value)))

(defun emux-screen-create (&optional properties terminal-name terminal-command)
  "Create new emux screen.  PROPERTIES is a property list that will be associated
with the created screen. The optional TERMINAL-NAME will name the default
screen terminal and TERMINAL-COMNMAND is the command to run in the newly
created terminal"
  (interactive)
  (let ((screen-symbol (gensym "emux-screen-")))
    (setplist screen-symbol properties)
    (emux-screen-save-current)
    (delete-other-windows)
    (emux-screen-current screen-symbol)
    (unless (emux-screen-get :no-terminal screen-symbol)
      (emux-term-create
       terminal-name
       terminal-command))
    (unless (emux-screen-get :name screen-symbol)
      (emux-screen-set :name "1" screen-symbol))
    (emux-set
     'screens
     (cons
      screen-symbol
      (emux-screens)))
    (emux-screen-current screen-symbol)))

(defun emux-screen-current (&optional screen)
  "Return current screen.  If optional SCREEN is passed in, it
will be set to current and then returned."
  (if screen
      (emux-set 'current-screen screen))
  (emux-get 'current-screen))

(defun emux-screen-rename (name &optional screen)
  "Change the name of screen SCREEN to NAME.  If SCREEN
is not included, use current screen."
  (interactive "snew screen name: ")
  (emux-screen-set :name name screen))

(defun emux-screen-switch (&optional screen)
  "Switch to a different screen.  If the option SCREEN is passed
in, switch to it, otherwise prompt for screen to switch to."
  (interactive)
  (emux-screen-save-current)
  (if (emux-screens)
      (let ((switch-to-screen
             (or
              screen
              (emux-screen-from-name
               (car
                (split-string
                 (emux-completing-read
                  "screen: "
                  (mapcar
                   (lambda (screen)
                     (format
                      "%s[%s]"
                      (or (emux-screen-get :name screen) "")
                      (mapconcat
                       (lambda (buffer)
                         (cadr (split-string (buffer-name buffer) "/")))
                       (emux-screen-get :buffers screen) ":")))
                   (emux-screens))) "\\["))))))
        (set-window-configuration (emux-screen-get :config  switch-to-screen))
        (emux-screen-current switch-to-screen)
        (message (format "switched to screen '%s'" (emux-screen-get :name))))
    (message "there are no screens to switch to.")))

(defun emux-screen-from-name (name)
  "Internal emux use, return screen with name NAME."
  (catch 'break
    (mapc (lambda (screen)
            (if (equal name (emux-screen-get :name screen))
                (throw 'break screen)))
          (emux-screens))))

(defun emux-screen-destroy (&optional screen)
  "Destroy screen SCREEN and the buffers associated with it.
If the optional SCREEN parameter is not passed, use current screen."
  (interactive)
  (let ((screen (or screen (emux-screen-current))))
    (mapc 'emux-term-destroy (emux-screen-get :buffers screen))
    (emux-set 'screens (remove screen (emux-screens)))
    (if (eq screen (emux-screen-current))
        (when (emux-screens)
          (emux-screen-switch (car (emux-screens)))))))

(defun emux-screen-save-current ()
  "Internal emux use, Save the configuration and buffers of the current screen."
  (when (emux-screen-current)
    (emux-screen-set :config (current-window-configuration))
    (emux-screen-set :buffers (mapcar 'window-buffer (window-list)))))

(defadvice emux-session-switch (after emux-session-switch-to-current-screen activate)
  "Switch to a sessions current screen after switching sessions."
  (emux-screen-switch (emux-session-get :current-screen)))

(defadvice emux-term-create (after emux-screen-save-current)
  "Save state after a new terminal is created"
  (emux-screen-save-current))

(provide 'emux-screen)
