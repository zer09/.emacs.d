(defun setup-lisp ()
  ;; (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (when (require 'easy-escape nil t)
    (easy-escape-minor-mode))
  (trycall #'aggressive-indent-mode)
  (setq-local tab-width 8))

(defun setup-emacs-lisp ()
  (setup-lisp)
  (trycall #'nameless-mode))

(add-hook 'lisp-mode-hook 'setup-lisp)
(add-hook 'emacs-lisp-mode-hook 'setup-emacs-lisp)
