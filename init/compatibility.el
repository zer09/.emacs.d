(provide 'compatibility)

(defmacro when-os (os &rest body)
  (declare (indent defun)
           (debug t))
  `(when (if (listp ,os) (memq system-type ,os) (eq system-type ,os))
     ,@body))

(defmacro optionally (&rest body)
  "Run BODY without interrupting loading upon failure."
  (declare (indent defun))
  `(ignore-errors
     ,@body))

(defmacro trycall (func &rest args)
  "Call (FUNC ARGS) is FUNC is a bound function symbol."
  (declare (debug t))
  `(when (fboundp (quote ,func))
     (,func ,@args)))

(when (< emacs-major-version 25)
  (when-os 'gnu/linux
    ;; rsync packages to elpa-(version), including only the .el files.
    (setq package-user-dir (expand-file-name (format "elpa-%d" emacs-major-version) user-emacs-directory))
    (let* ((sync-fmt "rsync -a --delete ~/.emacs.d/elpa/ '%s' --filter='-p *.elc' --filter='-p __pycache__/'")
           (sync-cmd (format sync-fmt package-user-dir)))
      (unless (eq (shell-command sync-cmd) 0)
        (warn "Syncing packages (%s) failed" sync-cmd))))
  (when (string-equal emacs-version "24.3.1")
    (init-load-file "compat244.el"))
  (init-load-file "compat25.el"))

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; compatibility.el ends here
