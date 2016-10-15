(setq-default writeroom-mode-line
              `(,(propertize " " 'display '(space :align-to 0))
                mode-line-front-space mode-line-modified
                " " mode-line-buffer-identification
                " " (vc-mode vc-mode)
                "  🔋" battery-mode-line-string
                "  🕟 " display-time-string))

(defvar ~/writeroom/saved-size nil)

(defun ~/writeroom/adjust-font-size ()
  (setq-local text-scale-mode-amount 2)
  (text-scale-mode))

(defun ~/writeroom/reset-font-size ()
  (kill-local-variable 'text-scale-mode-amount)
  (text-scale-mode -1))

(with-eval-after-load 'writeroom-mode
  (require 'face-remap)
  (setq writeroom-global-effects
        (delq 'writeroom-set-fullscreen writeroom-global-effects))
  (advice-add 'writeroom--enable :before #'~/writeroom/adjust-font-size)
  (advice-add 'writeroom--disable :after #'~/writeroom/reset-font-size))
