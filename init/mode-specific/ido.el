(setq-default ido-everywhere t
              ido-enable-flex-matching t
              ido-case-fold t
              ido-auto-merge-work-directories-length -1
              ido-create-new-buffer 'always)

(ido-mode)
(add-hook 'after-init-hook '(lambda () (trycall ido-ubiquitous-mode)))
