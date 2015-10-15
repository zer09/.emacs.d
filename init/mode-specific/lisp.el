(defun setup-lisp ()
  ;; (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (optionally
    (require 'easy-escape)
    (easy-escape-minor-mode))
  (aggressive-indent-mode)
  (setq-local tab-width 8))

(defun setup-emacs-lisp ()
  (setup-lisp)
  (optionally (nameless-mode)))

(add-hook 'lisp-mode-hook 'setup-lisp)
(add-hook 'emacs-lisp-mode-hook 'setup-emacs-lisp)
