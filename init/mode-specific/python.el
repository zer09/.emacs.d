(with-eval-after-load 'elpy
  (put 'pyvenv-workon 'safe-local-variable #'stringp)

  (let ((elpy-disabled '(elpy-module-highlight-indentation elpy-module-flymake)))
    (setq-default elpy-rpc-backend "jedi"
                  elpy-modules (cl-loop for x in elpy-modules
                                        unless (memq x elpy-disabled)
                                        collect x)))


  (let ((elpy-disabled-bindings '("<S-return>" "<C-S-return>" "<C-return>" "M-TAB"
                                  "<C-down>" "<C-up>" "<C-left>" "<C-right>"
                                  "<M-down>" "<M-up>" "<M-left>" "<M-right>")))
    (cl-loop for binding in elpy-disabled-bindings
             do (define-key elpy-mode-map (kbd binding) nil)))

  (when-os 'windows-nt
    (setq-default python-shell-interpreter "pythonw"))
  (when-os 'gnu/linux
    (setq-default python-shell-interpreter "python3")))

;; (defun my-python-display-errors (errs)
;;   "Clean up messy Python ERRS."
;;   (when (and errs (flycheck-may-use-echo-area-p))
;;     (display-message-or-buffer (mapconcat #'flycheck-error-format-message-and-id errs "\n")
;;                                flycheck-error-message-buffer)))

(defun setup-elpy ()
  "Setup elpy."
  (require 'python-prettify)
  (setq-local prettify-symbols-alist python-prettify-symbols-alist)
  (prettify-symbols-mode)

  (let* ((buffer-path (buffer-file-name))
         (true-path   (and buffer-path (file-truename buffer-path))))
    (if (and true-path (string-match-p (regexp-opt '("afs")) true-path))
        (progn (elpy-mode -1)
               (eldoc-mode -1)
               (hs-minor-mode -1)
               (unload-feature 'elpy))
      (elpy-enable)
      (elpy-mode))))

(add-hook 'python-mode-hook #'setup-elpy)
