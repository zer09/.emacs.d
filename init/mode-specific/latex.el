(with-eval-after-load 'latex
  (add-to-list 'TeX-command-list '("Make" "make" TeX-run-compile nil t))

  (setq-default TeX-master t ;; Use current file as master; overwrite using .dir-locals.el if needed
                TeX-engine 'xetex
                reftex-plug-into-AUCTeX t
                LaTeX-verbatim-environments-local '("lstlisting")
                TeX-command-extra-options "-shell-escape"))

(defun LaTeX-wrap-in-math (start end)
  (interactive "r")
  (when (and (region-active-p) start end)
    (let ((math (buffer-substring start end)))
      (delete-region start end)
      (insert (concat "\\(" math "\\)")))))

(defun setup-auctex ()
  (interactive)

  (require 'greek-prettify)
  (setq prettify-symbols-alist prettify-symbols-greek-alist)
  (define-key LaTeX-mode-map (kbd "C-c w") #'LaTeX-wrap-in-math)

  (prettify-symbols-mode)
  (flyspell-mode)
  (flycheck-mode)
  (yas-minor-mode)
  (yas-reload-all)

  (TeX-source-correlate-mode)
  (company-auctex-init)
  (turn-on-reftex))

(add-to-list 'auto-mode-alist '("\\.tex$" . LaTeX-mode))
(add-hook 'LaTeX-mode-hook #'setup-auctex)
