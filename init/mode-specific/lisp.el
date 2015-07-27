(defun setup-lisp ()
  (when (require 'easy-escape nil t)
    (easy-escape-minor-mode))
  (aggressive-indent-mode)
  (setq-local tab-width 8))

(add-hook 'lisp-mode-hook 'setup-lisp)
(add-hook 'emacs-lisp-mode-hook 'setup-lisp)
