(defun setup-csharp ()
  (setq-local c-basic-offset 2)
  (when-os 'gnu/linux
    (require 'omnisharp "~/.emacs.d/lisp/omnisharp-emacs/omnisharp.el" t)
    (omnisharp-mode)))

(with-eval-after-load 'csharp-mode
  (setq-default csharp-want-imenu nil)) ;; Horribly slow

(with-eval-after-load 'omnisharp
  (setq-default omnisharp-server-executable-path "/build/omnisharp-roslyn/scripts/Omnisharp"
                omnisharp-eldoc-support nil)
  (define-key omnisharp-mode-map (kbd "M-.") #'omnisharp-go-to-definition))

(add-hook 'csharp-mode-hook #'setup-csharp)
