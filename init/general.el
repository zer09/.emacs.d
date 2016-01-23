;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General customization ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; GC
(setq-default gc-cons-threshold (* 8 1000 1000))

;; Appearance
(setq-default ;initial-major-mode 'fundamental-mode
              initial-frame-alist '((fullscreen . maximized)) ;; Start in full screen (see also -mm)
              frame-title-format '((:eval (cond ((buffer-modified-p) "*")
                                                (buffer-read-only "%%")
                                                (t ""))) "%b")
              cursor-type 'bar
              x-gtk-use-system-tooltips nil)

(tool-bar-mode -1)
(blink-cursor-mode)
(column-number-mode)
(trycall #'set-fringe-mode '(8 . 8))
(trycall #'scroll-bar-mode -1)
(setq-default overlay-arrow-string "")

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
(winner-mode)
(xterm-mouse-mode)
(setq-default tooltip-delay 0.3
              set-mark-command-repeat-pop t
              fast-but-imprecise-scrolling t
              eval-expression-print-length nil
              eval-expression-print-level nil)


(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(savehist-mode)
(trycall #'save-place-mode)

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
                            (eldoc-mode)
                            ;; (ruler-mode)
                            ;; (which-function-mode)
                            (trycall #'flycheck-mode)
                            (trycall #'ws-butler-mode)))

;; All text modes
(add-hook 'text-mode-hook (lambda ()
                            ;; (ruler-mode)
                            (trycall #'flyspell-mode)
                            (trycall #'ws-butler-mode)
                            (visual-line-mode)))

;;; Disabled functions
(put 'dired-find-alternate-file 'disabled nil)
