(provide 'compat244)

(defun macrop (object)
  "Non-nil if and only if OBJECT is a macro."
  (let ((def (indirect-function object)))
    (when (consp def)
      (or (eq 'macro (car def))
          (and (autoloadp def) (memq (nth 4 def) '(macro t)))))))

(defmacro with-eval-after-load (file &rest body)
  "Execute BODY after FILE is loaded.
FILE is normally a feature name, but it can also be a file name,
in case that file does not provide any feature."
  (declare (indent 1) (debug t))
  `(eval-after-load ,file (lambda () ,@body)))

(defalias 'function-put
  ;; We don't want people to just use `put' because we can't conveniently
  ;; hook into `put' to remap old properties to new ones.  But for now, there's
  ;; no such remapping, so we just call `put'.
  #'(lambda (function prop value)
      "Set FUNCTION's property PROP to VALUE.
The namespace for PROP is shared with symbols.
So far, FUNCTION can only be a symbol, not a lambda expression."
      (put function prop value)))

(defalias 'save-mark-and-excursion 'save-excursion)

;;; compat244.el end here
