(defun setup-eww ()
  (interactive)
  (setq-local show-trailing-whitespace nil))

(setq-default network-security-level 'high)
(add-hook 'eww-mode-hook #'setup-eww)
