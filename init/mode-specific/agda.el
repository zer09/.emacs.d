(require 'agda2 "~/.cabal/share/x86_64-linux-ghc-7.8.3/Agda-2.4.3/emacs-mode/agda2.el" t)

(defun setup-agda ()
  (require 'agda-prettify)
  (add-to-list 'agda2-include-dirs "/build/agda-stdlib/src/")
  (customize-set-variable 'agda2-highlight-face-groups 'default-faces)
  (setq prettify-symbols-alist agda-prettify-symbols-alist)
  (prettify-symbols-mode))

(add-hook 'agda2-mode-hook #'setup-agda)
