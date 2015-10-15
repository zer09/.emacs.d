;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General customization ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Envoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; Don't load outdated elc files
(setq-default load-prefer-newer t)

;; GC
(setq-default gc-cons-threshold (* 8 1000 1000))

;; Appearance
(setq-default initial-major-mode 'fundamental-mode
              initial-frame-alist '((fullscreen . maximized)) ;; Start in full screen (see also -mm)
              cursor-type 'bar)
(tool-bar-mode -1)
(column-number-mode)
(when (fboundp 'set-fringe-mode)
  (set-fringe-mode '(8 . 8)))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
;; (set-face-attribute 'fringe nil :background "#FFFFFF") ;; Set by theme

;; Interaction
(setq-default visible-bell t
              ring-bell-function 'ignore)

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

(setq-default tooltip-delay 0.3)

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

(optionally (global-page-break-lines-mode))

;; All program modes
(add-hook 'prog-mode-hook (lambda ()
                            (hs-minor-mode)
                            (eldoc-mode)
                            (which-function-mode)
                            (optionally (flycheck-mode))
                            (optionally (ws-butler-mode))))

;; All text modes
(add-hook 'text-mode-hook (lambda ()
                            (optionally (flyspell-mode))
                            (optionally (ws-butler-mode))
                            (visual-line-mode)))
