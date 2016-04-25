(with-eval-after-load 'markdown-mode
  ;; (define-key markdown-mode-map (kbd "C-c t") 'today)
  ;; (define-key markdown-mode-map (kbd "C-c n") 'now)
  (setq-default markdown-command "pandoc --mathjax --standalone"))

(defun setup-markdown ()
  (flyspell-mode)
  (electric-indent-local-mode -1))

(add-hook 'markdown-mode-hook #'setup-markdown)
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
