;;;;;;;;;;;;;;;;;;;;;;
;; Package settings ;;
;;;;;;;;;;;;;;;;;;;;;;

(setq-default package-enable-at-startup nil) ;; Cask loads packages before startup
(load "~/.cask/cask")
(cask-initialize)
