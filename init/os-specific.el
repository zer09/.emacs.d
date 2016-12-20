;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows-specific config ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (member system-type '(windows-nt cygwin))
  (set-face-attribute 'default nil :family "Ubuntu Mono")
  (set-font-size-in-all-fontsets 176))

;; (setq-default explicit-shell-file-name
;; "C:\\MinGW\\msys\\1.0\\msys-headless.bat"))
