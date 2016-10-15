(defun setup-lisp ()
  ;; (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (when (require 'easy-escape nil t)
    (easy-escape-minor-mode))
  (aggressive-indent-mode)
  (setq-local tab-width 8))

(defun setup-emacs-lisp ()
  (setup-lisp)
  (nameless-mode))

(add-hook 'lisp-mode-hook 'setup-lisp)
(add-hook 'emacs-lisp-mode-hook 'setup-emacs-lisp)

;; From http://endlessparentheses.com/eval-result-overlays-in-emacs-lisp.html
(autoload 'cider--make-result-overlay "cider-overlays")

(setq-default cider-eval-result-prefix "⇒ ")

(defun endless/eval-overlay (value point)
  (cider--make-result-overlay (format "%S" value)
    :where point
    :duration 'command)
  ;; Preserve the return value.
  value)

(advice-add 'eval-region :around
            (lambda (f beg end &rest r)
              (endless/eval-overlay
               (apply f beg end r)
               end)))

(advice-add 'eval-last-sexp :filter-return
            (lambda (r)
              (endless/eval-overlay r (point))))

(advice-add 'eval-defun :filter-return
            (lambda (r)
              (endless/eval-overlay
               r
               (save-excursion
                 (end-of-defun)
                 (point)))))
