(require 'flycheck "~/.emacs.d/lisp/flycheck/flycheck.el" t)
(require 'flycheck-pos-tip "~/.emacs.d/lisp/flycheck-pos-tip/flycheck-pos-tip.el" t)

(defun my-flycheck-locate-config-file (fname &rest _)
  (let* ((config-dir (expand-file-name "external-config" user-emacs-directory))
         (abs-name   (expand-file-name fname config-dir)))
    (when (file-exists-p abs-name)
      abs-name)))

(defun my-flycheck-mode-line-status-text (&optional status)
  "Get a text describing STATUS for use in the mode line."
  (format
   "  [ %s]" (pcase (or status flycheck-last-status-change)
               (`not-checked "")
               (`no-checker "⛭ ")
               (`running "⚙ ")
               (`errored "✘ ")
               (`finished
                (let ((errors-warnings (flycheck-count-errors flycheck-current-errors)))
                  (let* ((errors (cdr (assq 'error errors-warnings)))
                         (warnings (cdr (assq 'warning errors-warnings)))
                         (errors-str (when errors (format " ⚐ %d" errors)))
                         (warnings-str (when warnings (format " ⚠ %d" warnings))))
                    (cond
                     ((and errors warnings) (concat errors-str " " warnings-str))
                     (warnings warnings-str)
                     (errors errors-str)
                     (t "✓ ")))))
               (`interrupted "-")
               (`suspicious "?"))))


(with-eval-after-load 'flycheck
  (trycall #'flycheck-pos-tip-mode)
  (setq-default flycheck-mode-line '(:eval (my-flycheck-mode-line-status-text)))
  (setq-default flycheck-checkers (cons 'python-pylint (remove 'python-pylint flycheck-checkers)))
  (add-to-list 'flycheck-locate-config-file-functions #'my-flycheck-locate-config-file))
