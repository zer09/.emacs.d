;;;;;;;;;;;;;;;;;;;;;;
;; Package settings ;;
;;;;;;;;;;;;;;;;;;;;;;

;; http://stackoverflow.com/questions/11127109/emacs-24-package-system-initialization-problems
(setq-default package-enable-at-startup nil) ;; Load packages immediately

;; (package-initialize)

(load "~/.cask/cask")
(cask-initialize)

(when-os 'gnu/linux
  (when (< emacs-major-version 25)
    ;; Packages are mirrored on a separate folder; re-compile them
    (byte-recompile-directory package-user-dir)))
