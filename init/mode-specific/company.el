(defun ~/try-indent ()
  "Try indenting current line; return non-nil if something happened."
  (unless (memq indent-line-function '(indent-relative indent-relative-maybe))
    (let ((old-point (point))
          (old-tick (buffer-chars-modified-tick))
          (tab-always-indent t)
          (python-indent-trigger-commands
           (cons this-command (bound-and-true-p python-indent-trigger-commands))))
      (call-interactively #'indent-for-tab-command)
      (not (and (eq old-point (point))
                (eq old-tick (buffer-chars-modified-tick)))))))

(defun ~/try-expand-snippet ()
  "Try expanding snippet at point; return non-nil if something happened."
  (let ((yas-fallback-behavior nil)
        (yas-enabled (and (featurep 'yasnippet)
                          (or (bound-and-true-p yas-mode)
                              (bound-and-true-p yas-minor-mode))))
        (next-char-is-word (eq (syntax-class (syntax-after (point))) 2)))
    (and yas-enabled
         (not next-char-is-word)
         (yas-expand))))

(defun ~/try-complete-common ()
  "Try completing to common prefix; return non-nil if something happened."
  ;; This bit of code added to compute the common prefix based only on visible entries.
  (require 'dash)
  (when (bound-and-true-p company-backend)
    (let* ((height (company--pseudo-tooltip-height))
           (visible-candidates (when (> height 1) (-take height company-candidates)))
           (company-common (if visible-candidates
                               (try-completion "" visible-candidates)
                             company-common)))
      (call-interactively #'company-complete-common)
      ;; (company-manual-begin) ;; disabled since in shell-mode it offers ../ first (reported as a pcomplete bug)
      )))

(defun ~/indent-or-yasnippet-or-completion ()
  "Do a reasonable action for tab command."
  (interactive)
  (or (~/try-indent)
      (~/try-expand-snippet)
      (~/try-complete-common)))

(with-eval-after-load 'yasnippet
  (substitute-key-definition 'yas-expand
                             '~/indent-or-yasnippet-or-completion
                             yas-minor-mode-map))

(with-eval-after-load 'company
  (setq-default company-idle-delay 0.01
                company-tooltip-align-annotations t
                company-dabbrev-code-everywhere t
                company-dabbrev-downcase nil
                company-require-match 'never)
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  (substitute-key-definition 'company-complete-common
                             '~/indent-or-yasnippet-or-completion
                             company-active-map))

(add-hook 'after-init-hook 'global-company-mode)
