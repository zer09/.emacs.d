(setq-default ido-everywhere t
              ido-enable-flex-matching t
              ido-case-fold t
              ido-auto-merge-work-directories-length -1
              ido-default-buffer-method 'selected-window
              ido-default-file-method 'selected-window
              ido-create-new-buffer 'always)

(ido-mode)
(add-hook 'after-init-hook #'ido-ubiquitous-mode)

;; flx-ido was not the best
;; ;; (flx-ido-mode 1)
;; ;; disable ido faces to see flx highlights.
;; ;; (setq ido-use-faces nil)
