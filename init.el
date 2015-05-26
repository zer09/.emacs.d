(defconst init-dir "~/.emacs.d/init/")

(defun load-init-file (name)
  (load-file (expand-file-name name init-dir)))

(when (< emacs-major-version 25)
  (when (string-equal emacs-version "24.3.1")
    (load-init-file "compat244.el"))
  (load-init-file "compat25.el"))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (csharp-mode auto-complete wgrep-ag ag windresize wgrep diminish multiple-cursors latex-extra f ws-butler visual-regexp tuareg smex rainbow-delimiters popup markdown-mode magit latex-preview-pane julia-mode json-rpc ido-ubiquitous flycheck-ocaml expand-region epc elpy company-math company-auctex ace-jump-mode))))

(add-to-list 'load-path "~/.emacs.d/lisp/")

(defun load-init-files ()
  (interactive)
  (load-init-file "package.el")
  (load-init-file "general.el")
  (load-init-file "server.el")
  (load-init-file "fonts.el")
  (load-init-file "defuns.el")
  (load-init-file "modes-config.el")
  (load-init-file "keybindings.el"))

;; (cl-loop for x in (directory-files init-dir nil "\\.el\\'") collect `(load-init-file ,x))

(load-init-files)

;; Custom theme
(load-theme 'tangomod-dark t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(proof-locked-face ((t (:background "blue" :underline t)))))
(put 'narrow-to-region 'disabled nil)
