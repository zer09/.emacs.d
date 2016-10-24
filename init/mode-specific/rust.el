(setq-default racer-rust-src-path "/build/rust/src")

(defun ~/rust ()
  (setq-local parens-require-spaces nil)
  (setq-local prettify-symbols-alist '(("->" . ?→)
                                       ("=>" . ?⇒)))
  (racer-mode)
  (flycheck-rust-setup)
  (prettify-symbols-mode))

(add-hook 'rust-mode-hook #'~/rust)
