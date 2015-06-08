;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows-specific config ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (member system-type '(windows-nt cygwin))
  (set-face-attribute 'default nil
                      :inherit nil
                      :height 108
                      :family "Consolas"))
  ;; (setq-default explicit-shell-file-name
                ;; "C:\\MinGW\\msys\\1.0\\msys-headless.bat"))
