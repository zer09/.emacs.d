(defun setup-comint ()
  (setq-local show-trailing-whitespace nil))

(add-hook 'comint-mode-hook #'setup-comint)
