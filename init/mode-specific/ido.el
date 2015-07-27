(ido-mode)
(add-hook 'after-init-hook 'ido-ubiquitous-mode)

(setq-default ido-everywhere t
              ido-enable-flex-matching t
              ido-case-fold t
              ido-auto-merge-work-directories-length -1
              ido-create-new-buffer 'always)
