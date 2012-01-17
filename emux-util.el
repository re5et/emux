(defun emux-session-load-template ()
  (interactive)
  (let ((template
         (emux-completing-read
          "load emux session template: "
          (mapcar (lambda (filename)
                    (substring filename 0 -3))
                  (directory-files
                   "~/.emacs.d/emux-templates" nil "^[^#].*\.el")))))
    (if (cond
         ((not (emux-session-from-name template)) t)
         (t (yes-or-no-p (format "session %s already exists, really load?" template))))
        (progn
          (emux-session-create `(:name ,template))
          (load-file
           (concat "~/.emacs.d/emux-templates/"
                   template
                   ".el"))
          (message "%s template loaded" template)))))

(provide 'emux-util)
