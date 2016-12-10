(setq-default ido-everywhere t
              ido-enable-flex-matching t
              ido-case-fold t
              ido-auto-merge-work-directories-length -1
              ido-default-buffer-method 'selected-window
              ido-default-file-method 'selected-window
              ido-create-new-buffer 'always)

(ido-mode)
(add-hook 'after-init-hook #'ido-ubiquitous-mode)
(add-hook 'ido-setup-hook
          (lambda ()
            ;; ido-completion-map is a variable reset at every invocation of IDO
            (define-key ido-completion-map (kbd "<C-backspace>") nil)))

;; flx-ido was not the best
;; ;; (flx-ido-mode 1)
;; ;; disable ido faces to see flx highlights.
;; ;; (setq ido-use-faces nil)
