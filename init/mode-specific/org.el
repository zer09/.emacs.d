(with-eval-after-load 'org
  (setq-default org-log-done 'time
                org-support-shift-select t
                org-use-fast-todo-selection 'prefix
                org-todo-keywords '((sequence "TODO(t)" "IN PROGRESS(p)" "LATER(l)" "|" "DONE(d)"))
                org-latex-listings t))

(defun org-beamer-headless ()
  "Export current file to LaTeX, ommitting the preamble."
  (interactive)
  (require 'org)
  (let* ((fname (or buffer-file-name ""))
         (org-fname (replace-regexp-in-string "\.org\\'" ".tex" fname))
         (org-export-show-temporary-export-buffer nil))
    (if (string= fname org-fname)
        (error "Not sure where to save the LaTeX file")
      (org-beamer-export-as-latex nil nil nil t nil)
      (with-current-buffer "*Org BEAMER Export*"
        ;; (indent-region (point-min) (point-max)) ;; Breaks verbatim
        (set-buffer-file-coding-system 'utf-8)
        (write-file org-fname)
        (kill-buffer)))))

;; (setq-local user-full-name "Clément Pit-\\kern0pt-Claudel") ;; Doesn't seem to work

(add-hook 'org-mode-hook #'flyspell-mode)
