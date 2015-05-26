;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General customization ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default load-prefer-newer t) ;; Don't load outdated elc files

;; GC
(setq-default gc-cons-threshold (* 8 1000 1000))

;; Appearance
(setq-default initial-major-mode 'fundamental-mode
              initial-frame-alist '((fullscreen . maximized))) ;; Start in full screen (see also -mm)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode)
(fringe-mode '(8 . 8))
;; (set-face-attribute 'fringe nil :background "#FFFFFF") ;; Set by theme

;; Interaction
(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default inhibit-startup-message t
              inhibit-startup-echo-area-message t
              initial-scratch-message nil
              confirm-nonexistent-file-or-buffer nil)

(setq kill-buffer-query-functions ;; "This buffer has a live process..."
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; Behaviour
(xterm-mouse-mode)
(setq-default fast-but-imprecise-scrolling t)

;; Backups and temp files
(setq-default backup-directory-alist `(("." . "~/.emacs-backups"))
              backup-by-copying t
              create-lockfiles nil)

;; Editing
(setq-default tab-width 4
              fill-column 80
              indent-tabs-mode nil
              show-paren-style 'expression
              show-trailing-whitespace t)
(show-paren-mode)
(delete-selection-mode) ;; Replace selected text upon typing

;; All program modes
(add-hook 'prog-mode-hook (lambda ()
                            (hs-minor-mode)
                            (eldoc-mode)
                            (flycheck-mode)
                            (ws-butler-mode)
                            (which-function-mode)))

;; All text modes
(add-hook 'text-mode-hook (lambda ()
                            (flyspell-mode)
                            (ws-butler-mode)
                            (visual-line-mode)))
