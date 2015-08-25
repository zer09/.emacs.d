;;; Custom
(setq custom-file "~/.emacs.d/init/custom.el")

;;; Custom theme
(load-theme 'tangomod-dark t)

;;; Disable automatic package loading (must be kept here)
;; (package-initialize)

;;; Tools for loading and recompiling init files
(require 'bytecomp)
(defconst init-dir "~/.emacs.d/init/")

(defun load-with-optional-compilation (path compile)
  (let* ((elc      (byte-compile-dest-file path))
         (_        (when compile (byte-recompile-file path nil 0)))
         (load-elc (and  compile (file-exists-p elc))))
    (load (if load-elc elc path) nil t)))

(defun load-init-file (relative-path)
  (let* ((start-time (current-time))
         (path       (expand-file-name relative-path init-dir))
         (fname      (file-name-nondirectory relative-path)))
    (if (file-exists-p path)
        (progn (load-with-optional-compilation path (= emacs-major-version 25))
               (message "%.2fs [%s]" (float-time (time-since start-time)) fname))
      (message "[%s] skipped" fname))))

;;; Basic initialization
(load-init-file "compatibility.el")
(load-init-file "package.el")

(add-to-list 'load-path "~/.emacs.d/lisp/")

(defun load-init-files ()
  (interactive)
  (load-init-file "custom.el")
  (load-init-file "general.el")
  (load-init-file "fonts.el")
  (load-init-file "defuns.el")
  (load-init-file "mode-specific.el")
  (load-init-file "os-specific.el")
  (load-init-file "keybindings.el")
  (load-init-file "autoloads.el")
  (load-init-file "properties.el")
  (load-init-file "local.el"))

(load-init-files)
(put 'dired-find-alternate-file 'disabled nil)
