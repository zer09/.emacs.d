(with-eval-after-load 'magit
  (setq-default magit-diff-refine-hunk 'all
                magit-push-always-verify nil
                magit-diff-arguments '("--minimal" "--ignore-all-space"))
  (let ((magit-faces '(magit-diff-added magit-diff-removed magit-diff-our magit-diff-base magit-diff-their
                                        magit-diff-context magit-diff-added-highlight magit-diff-removed-highlight
                                        magit-diff-our-highlight magit-diff-base-highlight magit-diff-their-highlight
                                        magit-diff-context-highlight)))
    (dolist (face magit-faces)
      (set-face-attribute face nil :foreground "#eeeeec")))
  (add-hook 'magit-mode-hook 'visual-line-mode))
