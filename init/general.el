;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General customization ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; GC
(setq-default gc-cons-threshold (* 16 1000 1000))

;; Appearance
(setq-default ;initial-major-mode 'fundamental-mode
 initial-frame-alist '((fullscreen . maximized)) ;; Start in full screen (see also -mm)
 frame-title-format '((:eval (cond ((buffer-modified-p) "*")
                                   (buffer-read-only "%%")
                                   (t ""))) "%b")
 cursor-type 'bar
 x-gtk-use-system-tooltips nil)

(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode)
(column-number-mode)
(trycall #'set-fringe-mode '(8 . 8))
(trycall #'scroll-bar-mode -1)
(setq-default overlay-arrow-string "")

(defface ~/ellipsis-face
  '((t (:slant normal :weight bold))) ;; :inherit font-lock-preprocessor-face
  "Face used to display ellipses."
  :group 'company-coq-faces)

;; Display hidden text as …
(let ((chars (mapcar (lambda (c) (make-glyph-code c '~/ellipsis-face)) " …")))
  (set-display-table-slot standard-display-table 4 (vconcat chars)))

;; Interaction
(setq-default visible-bell t
              ring-bell-function 'ignore)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default inhibit-startup-message t
              inhibit-startup-echo-area-message t
              initial-scratch-message nil
              confirm-nonexistent-file-or-buffer nil)

;; Behaviour
(winner-mode)
(xterm-mouse-mode)
(setq-default tooltip-delay 0.15
              confirm-kill-processes nil
              set-mark-command-repeat-pop t
              fast-but-imprecise-scrolling t
              eval-expression-print-level nil
              eval-expression-print-length nil
              print-quoted t
              print-gensym t
              save-interprogram-paste-before-kill t
              apropos-do-all t
              load-prefer-newer t)

(setq kill-buffer-query-functions ;; "This buffer has a live process..."
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(savehist-mode)
(trycall #'save-place-mode)

;; Backups and temp files
(setq-default backup-directory-alist '(("." . "~/.emacs-backups"))
              auto-save-file-name-transforms '((".*" "~/.emacs-backups/" t))
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

(require 'compact-docstrings "~/.emacs.d/lisp/compact-docstrings/compact-docstrings.el" t)

;; All program modes
(add-hook 'prog-mode-hook (lambda ()
                            (eldoc-mode)
                            (ruler-mode)
                            ;; (which-function-mode)
                            (flycheck-mode)
                            (flyspell-prog-mode)
                            (ws-butler-mode)
                            (compact-docstrings-mode)
                            ))

;; All text modes
(add-hook 'text-mode-hook (lambda ()
                            (ruler-mode)
                            (flyspell-mode)
                            (ws-butler-mode)
                            (visual-line-mode)
                            (adaptive-wrap-prefix-mode)))

(with-eval-after-load 'company ;; See https://github.com/company-mode/company-mode/issues/548#issuecomment-228488327
  (defun company-echo-show-when-idle (&optional getter)
    (company-echo-cancel)
    (setq company-echo-timer
          (run-with-timer company-echo-delay nil 'company-echo-show getter))))
