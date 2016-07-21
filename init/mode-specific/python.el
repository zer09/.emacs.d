(with-eval-after-load 'elpy
  (put 'pyvenv-workon 'safe-local-variable #'stringp)

  (let ((elpy-disabled '(elpy-module-highlight-indentation elpy-module-flymake)))
    (setq-default elpy-rpc-backend "jedi"
                  elpy-rpc-python-command "python3"
                  elpy-modules (cl-set-difference elpy-modules elpy-disabled)))

  (let ((elpy-disabled-bindings '("<S-return>" "<C-S-return>" "<C-return>" "M-TAB"
                                  "<C-down>" "<C-up>" "<C-left>" "<C-right>"
                                  "<M-down>" "<M-up>" "<M-left>" "<M-right>")))
    (dolist (binding elpy-disabled-bindings)
      (define-key elpy-mode-map (kbd binding) nil)))

  (when-os 'windows-nt
    (setq-default python-shell-interpreter "pythonw"))
  (when-os 'gnu/linux
    (setq-default python-shell-interpreter "python3")))

(defun setup-elpy ()
  "Setup elpy."
  (require 'python-prettify)
  (setq-local prettify-symbols-alist python-prettify-symbols-alist)
  (prettify-symbols-mode)

  (setq-local parens-require-spaces nil)

  (let* ((buffer-path (buffer-file-name))
         (true-path   (and buffer-path (file-truename buffer-path))))
    (if (and true-path (string-match-p (regexp-opt '("afs")) true-path))
        (progn (elpy-mode -1)
               (eldoc-mode -1)
               (hs-minor-mode -1))
      (elpy-enable)
      (elpy-mode))))

(add-hook 'python-mode-hook #'setup-elpy)
