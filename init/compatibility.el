(provide 'compatibility)

(defmacro when-os (os &rest body)
  (declare (indent defun)
           (debug t))
  `(when (if (listp ,os) (memq system-type ,os) (eq system-type ,os))
     ,@body))

(when (< emacs-major-version 25)
  (when-os 'gnu/linux
    (let ((sync-script (expand-file-name (format "rsync-packages.sh %d" emacs-major-version) user-emacs-directory))
          (user-dir    (expand-file-name (format "elpa-%d" emacs-major-version) user-emacs-directory)))
      (unless (eq (shell-command (print sync-script)) 0)
        (error "Syncing packages (%s) failed" sync-script))
      (setq package-user-dir user-dir)))
  (when (string-equal emacs-version "24.3.1")
    (load-init-file "compat244.el"))
  (load-init-file "compat25.el"))

;;; compatibility.el ends here
