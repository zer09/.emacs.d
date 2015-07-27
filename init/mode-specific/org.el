(with-eval-after-load 'org
  (setq-default org-log-done 'time
                org-support-shift-select t
                org-use-fast-todo-selection 'prefix
                org-todo-keywords '((sequence "TODO(t)" "IN PROGRESS(p)" "LATER(l)" "|" "DONE(d)"))))
;; (setq-local user-full-name "Clément Pit-\\kern0pt-Claudel") ;; Doesn't seem to work

(add-hook 'org-mode-hook #'flyspell-mode)
