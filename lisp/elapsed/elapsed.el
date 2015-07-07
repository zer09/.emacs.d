(require 'time)

(defvar elapsed--start-time nil)

(defcustom elapsed-format-string " [%M'%S\"]"
  "Format string used to display elapsed time in the modeline")

(defconst elapsed-display-form
  '(when elapsed--start-time
     (format-time-string elapsed-format-string (time-since elapsed--start-time))))

;;;#autoload
(defun elapsed-start-timer ()
  (interactive)
  (setq display-time-interval 1)
  (display-time-mode 1)
  (setq elapsed--start-time (current-time))
  (setq display-time-string-forms (list elapsed-display-form)))
  ;; (add-to-list 'display-time-string-forms elapsed-display-form t))

;;;#autoload
(defalias 'elapsed-reset-timer #'elapsed-start-timer)

(defun elapsed-cancel-timer ()
  (interactive)
  (setq elapsed--start-time nil)
  (setq display-time-interval (default-value 'display-time-interval)))

(provide 'elapsed)
;;; elapsed.el ends here
