;; (defun tuareg-breaks-which-function-mode ()
;;   (which-function-mode -1))

;; (add-hook 'tuareg-mode-hook #'tuareg-breaks-which-function-mode)

(defun ~/ocaml-setup ()
  (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
    (when (and opam-share (file-directory-p opam-share))
      (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
      (autoload 'merlin-mode "merlin" nil t nil)
      (setq merlin-command 'opam)))
  (setq merlin-error-after-save nil)
  (flycheck-ocaml-setup)
  (prettify-symbols-mode)
  (merlin-mode))

(add-hook 'tuareg-mode-hook #'~/ocaml-setup)
