;;; Custom
(setq custom-file "~/.emacs.d/init/custom.el")

;;; Custom theme
(load-theme 'tangomod-dark t)

;;; Disable automatic package loading (must be kept here)
;; (package-initialize)

;;; Tools for loading and recompiling init files
(require 'bytecomp)
(defconst init-dir "~/.emacs.d/init/")

(defmacro with-timer (message &rest body)
  "Show MESSAGE after running BODY."
  (declare (indent defun))
  `(let* ((start-time (current-time)))
     (prog1
         ,@body
       (message "%.2fs [%s]" (float-time (time-since start-time)) ,message))))

(defun init-load-file (relative-path)
  "Load `init-dir'/RELATIVE-PATH, possibly compiling it first."
  (let ((path (expand-file-name relative-path init-dir)))
    (if (file-exists-p path)
        (with-timer (file-name-nondirectory relative-path)
          (if (= emacs-major-version 25)
              (byte-recompile-file path nil 0 nil))
          (let ((bin-path (byte-compile-dest-file path)))
            (load (if (file-exists-p bin-path) bin-path path) nil t)))
      (warn "[%s] skipped" relative-path))))

;;; Basic initialization
(add-to-list 'load-path "~/.emacs.d/lisp/")

(defconst init-files '("custom.el"
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
                       "local.el"))

(setq-default load-prefer-newer t)
(mapc #'init-load-file init-files)

;; (require 'mu4e)
;; (require 'mu4e-contrib)
;; (setq mu4e-html2text-command 'mu4e-shr2text)
(put 'upcase-region 'disabled nil)
