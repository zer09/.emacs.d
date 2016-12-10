(with-eval-after-load 'latex
  (add-to-list 'TeX-command-list '("Make" "make" TeX-run-compile nil t))

  (setq-default TeX-master t ;; Use current file as master; overwrite using .dir-locals.el if needed
                TeX-engine 'xetex
                TeX-fold-auto t
                TeX-parse-self t ; Enable parse on load.
                TeX-auto-save t ; Enable parse on save.
                TeX-auto-local ".auctex"
                reftex-plug-into-AUCTeX t
                LaTeX-verbatim-environments '("verbatim" "verbatim*" "lstlisting")
                LaTeX-verbatim-macros-with-delims-local '("py")
                reftex-default-bibliography '("~/documents/academia/bibliography/readings.bib")
                ;; TeX-command-extra-options "-shell-escape"
                bibtex-align-at-equal-sign t
                bibtex-entry-format t))

(with-eval-after-load 'bibtex
  (add-to-list 'load-path "~/.emacs.d/lisp/biblio.el/")
  (require 'biblio)
  (add-to-list 'load-path "~/.emacs.d/lisp/bibtex-extensions/")
  (require 'bibtex-repository))

(defun LaTeX-wrap-in-math (start end)
  (interactive "r")
  (when (and (region-active-p) start end)
    (let ((math (buffer-substring start end)))
      (delete-region start end)
      (insert (concat "\\(" math "\\)")))))

(defun setup-auctex ()
  (interactive)

  (define-key LaTeX-mode-map (kbd "C-c w") #'LaTeX-wrap-in-math)
  (define-key LaTeX-mode-map (kbd "M-RET") #'LaTeX-insert-item)

  (prettify-symbols-mode)
  (flyspell-mode)
  (flycheck-mode)
  (yas-minor-mode)
  (yas-reload-all)

  (TeX-fold-mode)
  (TeX-source-correlate-mode)
  (company-auctex-init)
  (turn-on-reftex))

(add-to-list 'auto-mode-alist '("\\.tex$" . LaTeX-mode))
(add-hook 'LaTeX-mode-hook #'setup-auctex)
