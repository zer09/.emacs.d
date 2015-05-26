;;;;;;;;;;;
;; Modes ;;
;;;;;;;;;;;

;; Emails
(define-derived-mode eml-mode gfm-mode
  (defun tkx ()
    (interactive)
    (insert "Thanks for your nice comments! "))
  (defun cheers ()
    (interactive)
    (insert "Cheers,")
    (newline)
    (insert "Clément."))
  (defun synchronicity-thanks ()
    (interactive)
    (insert "If you found this answer useful, you may want to support future development by making a small donation! Head to http://synchronicity.sourceforge.net/contribute.html for more information."))
  (defun synchronicity-home ()
    (interactive)
    (insert "http://synchronicity.sourceforge.net/"))
  (defun save-and-quit ()
    (interactive)
    (save-buffers-kill-terminal 1))
  (global-set-key [\C-return] 'save-and-quit) ;; FIXME overwritten by keybindings.el
  (setq mode-name "EML"))

(add-to-list 'auto-mode-alist '("\\.eml\\'" . eml-mode))
