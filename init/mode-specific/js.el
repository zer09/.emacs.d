(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook #'setup-js)

(defun setup-js ()
  (add-to-list 'company-backends 'company-jquery))
