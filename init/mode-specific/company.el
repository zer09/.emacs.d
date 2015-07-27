(defun company-yasnippet-or-completion ()
  (interactive)
  (let ((yas-fallback-behavior nil)
        (yas-enabled (and (featurep 'yasnippet)
                          (or (bound-and-true-p yas-mode)
                              (bound-and-true-p yas-minor-mode)))))
    (unless (and yas-enabled (yas-expand))
      (call-interactively #'company-complete-common))))

(defun setup-company ()
  (local-set-key [\C-return] 'company-manual-begin)
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  (substitute-key-definition 'company-complete-common 'company-yasnippet-or-completion company-active-map))

(with-eval-after-load 'company
  (setq-default company-idle-delay 0
                company-tooltip-align-annotations t
                company-dabbrev-code-everywhere t
                company-require-match "never")
  (setq-default company-quickhelp-delay 0
                company-quickhelp-max-lines 10) ;; Slows everything down
  (diminish 'company-mode))

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'company-mode-hook #'setup-company)
