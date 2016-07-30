(setq-default writeroom-mode-line
              `(,(propertize " " 'display '(space :align-to 0))
                mode-line-front-space mode-line-modified
                " " mode-line-buffer-identification
                " " (vc-mode vc-mode)
                "  🔋" battery-mode-line-string
                "  🕟 " display-time-string))

(defvar ~/writeroom/saved-size nil)

(defun ~/writeroom/adjust-font-size ()
  (unless ~/writeroom/saved-size
    (setq ~/writeroom/saved-size (face-attribute 'default :height))
    (set-font-size-in-all-fontsets 188)))

(defun ~/writeroom/reset-font-size ()
  (when ~/writeroom/saved-size
    (set-font-size-in-all-fontsets ~/writeroom/saved-size)
    (setq ~/writeroom/saved-size nil)))

(with-eval-after-load 'writeroom-mode
  (setq writeroom-global-effects
        (delq 'writeroom-set-fullscreen writeroom-global-effects))
  (advice-add 'writeroom--enable :before #'~/writeroom/adjust-font-size)
  (advice-add 'writeroom--disable :after #'~/writeroom/reset-font-size))
