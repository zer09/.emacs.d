;; Custom theme
(load-theme 'tangomod-dark t)

(defconst init-dir "~/.emacs.d/init/")

(defun load-init-file (name)
  (load-file (expand-file-name name init-dir)))

(load-init-file "compatibility.el")
(load-init-file "package.el")
;; Must be kept here
;; (package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(frame-background-mode (quote dark))
 '(package-selected-packages
   (quote
    (emr highlight-symbol haskell-mode company ag markdown-mode company-math csharp-mode expand-region ido-ubiquitous julia-mode magit multiple-cursors yasnippet flycheck-package omnisharp wgrep-ag windresize wgrep diminish latex-extra ws-butler visual-regexp tuareg smex racket-mode rainbow-delimiters popup json-rpc epc elpy company-auctex ace-jump-mode))))

(add-to-list 'load-path "~/.emacs.d/lisp/")

(defun load-init-files ()
  (interactive)
  (load-init-file "general.el")
  (load-init-file "server.el")
  (load-init-file "fonts.el")
  (load-init-file "defuns.el")
  (load-init-file "modes-config.el")
  (load-init-file "keybindings.el")
  (load-init-file "os-specific.el")
  (load-init-file "autoloads.el"))

;; (cl-loop for x in (directory-files init-dir nil "\\.el\\'") collect `(load-init-file ,x))

(load-init-files)

(put 'narrow-to-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
