(provide 'compatibility)

(defmacro when-os (os &rest body)
  (declare (indent defun)
           (debug t))
  `(when (if (listp ,os) (memq system-type ,os) (eq system-type ,os))
     ,@body))

(defmacro unless-os (os &rest body)
  (declare (indent defun)
           (debug t))
  `(unless (if (listp ,os) (memq system-type ,os) (eq system-type ,os))
     ,@body))

(defmacro trycall (func &rest args)
  "Call (FUNC ARGS) is FUNC is a bound function symbol."
  (declare (debug t))
  `(when (fboundp ,func)
     (funcall ,func ,@args)))

(when (< emacs-major-version 25)
  (when (version< emacs-version "24.4")
    (~/load-file "compat244.el"))
  (~/load-file "compat25.el"))

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; compatibility.el ends here
