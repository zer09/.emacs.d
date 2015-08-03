(require 'flycheck "~/.emacs.d/lisp/flycheck/flycheck.el")

(defun my-flycheck-locate-config-file (fname &rest _)
  (let* ((config-dir (expand-file-name "external-config" user-emacs-directory))
         (abs-name   (expand-file-name fname config-dir)))
    (when (file-exists-p abs-name)
      abs-name)))

(with-eval-after-load 'flycheck
  (setq-default flycheck-display-errors-function #'flycheck-pos-tip-error-messages
                flycheck-checkers (cons 'python-pylint (remove 'python-pylint flycheck-checkers)))
  (add-to-list 'flycheck-locate-config-file-functions #'my-flycheck-locate-config-file))
