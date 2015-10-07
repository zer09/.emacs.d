(with-eval-after-load 'haskell-mode
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))

(with-eval-after-load 'haskell-cabal
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))

(with-eval-after-load 'haskell-mode ;; From the manual
  (define-key haskell-mode-map (kbd "C-x C-d") nil)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
  (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c M-.") nil)
  (define-key haskell-mode-map (kbd "C-c C-d") nil))

(require 'haskell-prettify "~/.emacs.d/lisp/prettify-alists/haskell-prettify.el" t)
(add-hook 'haskell-mode-hook 'haskell-prettify-enable)
(add-hook 'haskell-interactive-mode-hook 'haskell-prettify-enable)

(defun setup-haskell ()
  (haskell-indentation-mode)
  (add-hook 'completion-at-point-functions 'haskell-process-completions-at-point nil t))

(add-hook 'haskell-mode-hook #'setup-haskell)
