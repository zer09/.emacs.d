(defun setup-eww ()
  (interactive)
  (setq-local show-trailing-whitespace nil))

(add-hook 'eww-mode-hook #'setup-eww)
