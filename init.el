;;; Custom theme
(load-theme 'tangomod-dark t)

;;; Custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(frame-background-mode (quote dark))
 '(package-selected-packages
   (quote
    (smart-mode-line avy json-mode jinja2-mode aggressive-indent emr flycheck-pos-tip highlight-symbol haskell-mode company ag markdown-mode company-math csharp-mode expand-region ido-ubiquitous julia-mode magit multiple-cursors yasnippet flycheck-package omnisharp wgrep-ag windresize wgrep diminish latex-extra ws-butler visual-regexp tuareg smex racket-mode rainbow-delimiters popup json-rpc epc elpy company-auctex))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; Tools for loading and recompiling init files
(require 'bytecomp)
(defconst init-dir "~/.emacs.d/init/")

(defun load-init-file (name)
  (let ((start-time (current-time))
        (path       (expand-file-name name init-dir)))
    (when (file-exists-p path)
      (byte-recompile-file path nil 0 t)
      (message "[%s] %.2fs" name (float-time (time-since start-time))))))

;;; Disable automatic package loading (must be kept here)
;; (package-initialize)

;;; Basic initialization
(load-init-file "compatibility.el")
(load-init-file "package.el")

(add-to-list 'load-path "~/.emacs.d/lisp/")

(defun load-init-files ()
  (interactive)
  (load-init-file "local.el")
  (load-init-file "general.el")
  (load-init-file "server.el")
  (load-init-file "fonts.el")
  (load-init-file "defuns.el")
  (load-init-file "modes-config.el")
  (load-init-file "keybindings.el")
  (load-init-file "os-specific.el")
  (load-init-file "autoloads.el")
  (load-init-file "properties.el"))

(load-init-files)
