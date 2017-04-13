;;; Custom
(setq-default custom-file "~/.emacs.d/init/custom.el")
(setq-default load-prefer-newer t)

;;; Custom theme
(load-theme 'tangomod-dark t)
(setq-default frame-background-mode 'dark)

;;; Disable automatic package loading (must be kept here)
;; (package-initialize)

;;; Tools for loading and recompiling init files
;; (require 'bytecomp)
(defconst ~/init-dir "~/.emacs.d/init/")

(defmacro with-timer (message &rest body)
  "Show MESSAGE and elapsed time after running BODY."
  (declare (indent defun))
  `(let* ((start-time (current-time)))
     (prog1
         ,@body
       (message "%.4fs [%s]" (float-time (time-since start-time)) ,message))))

(defun ~/load-file (relative-path)
  "Load `~/init-dir'/RELATIVE-PATH."
  (let ((path (expand-file-name relative-path ~/init-dir)))
    (if (file-exists-p path)
        (with-timer (file-name-nondirectory relative-path)
          (load path))
      (warn "[%s] skipped" relative-path))))

;;; Basic initialization
(add-to-list 'load-path "~/.emacs.d/lisp/")

(defconst ~/init-files '(
                         "custom.el"
                         "compatibility.el"
                         "package.el"
                         "general.el"
                         "fonts.el"
                         "defuns.el"
                         "mode-specific.el"
                         "os-specific.el"
                         "keybindings.el"
                         "autoloads.el"
                         "properties.el"
                         "local.el"
                         ))

(mapc #'~/load-file ~/init-files)

(add-to-list 'load-path "/build/org-mode/lisp/")
(add-to-list 'load-path "/build/org-mode/contrib/lisp/")
(require 'org-loaddefs nil t)

;; (require 'mu4e)
;; (require 'mu4e-contrib)
;; (setq mu4e-html2text-command 'mu4e-shr2text)
(put 'erase-buffer 'disabled nil)
