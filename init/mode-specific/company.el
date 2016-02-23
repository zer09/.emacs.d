(defun company-yasnippet-or-completion ()
  (interactive)
  (require 'dash)
  (let ((yas-fallback-behavior nil)
        (yas-enabled (and (featurep 'yasnippet)
                          (or (bound-and-true-p yas-mode)
                              (bound-and-true-p yas-minor-mode)))))
    (setq company-common
          ;; This bit of code added to compute the common prefix based only on visible entires.
          (-if-let* ((height (company--pseudo-tooltip-height))
                     (visible-candidates (when (> height 1) (-take height company-candidates))))
              (try-completion "" visible-candidates)
            company-common))
    (unless (and yas-enabled (yas-expand))
      (call-interactively #'company-indent-or-complete-common))))

(defun setup-company ()
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  (substitute-key-definition 'company-complete-common 'company-yasnippet-or-completion company-active-map))

(with-eval-after-load 'company
  (setq-default company-idle-delay 0
                company-tooltip-align-annotations t
                company-dabbrev-code-everywhere t
                company-dabbrev-downcase nil
                company-require-match 'never)
  (setq-default company-quickhelp-delay 0.5 ;; Slows everything down
                company-quickhelp-max-lines 10))

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'company-mode-hook #'setup-company)
