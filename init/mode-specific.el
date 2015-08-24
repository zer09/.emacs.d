;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;;; easy-escape

(require 'easy-escape "~/.emacs.d/lisp/easy-escape/easy-escape.el" t)

;;; prettify

(add-to-list 'load-path "~/.emacs.d/lisp/prettify-alists/")

;;; TRAMP

(when-os 'windows-nt
  (setq tramp-mode nil))

;; Presentations
(require 'demo-mode  "~/.emacs.d/lisp/demo-mode/demo-mode.el" t)

;;; Email
(require 'eml-mode "~/.emacs.d/lisp/bits-n-pieces/eml-mode.el" t)

;;; Elapsed time (stopwatch)

(require 'elapsed "~/.emacs.d/lisp/elapsed/elapsed.el" t)

;;; Spelling
(when-os 'windows-nt
  (setq-default ispell-program-name "c:/Program Files (x86)/Aspell/bin/aspell.exe"))

;;; Compilation

(setq-default compilation-scroll-output 'first-error)

;;; Recentf

(add-hook 'after-init-hook 'recentf-mode)

;;; multiple-cursors

(with-eval-after-load 'multiple-cursors-core
  (define-key mc/keymap (kbd "M-T") 'mc/reverse-regions)
  (define-key mc/keymap (kbd "C-,") 'mc/unmark-next-like-this)
  (define-key mc/keymap (kbd "C-.") 'mc/skip-to-next-like-this))

;;; Powerline

(setq sml/theme 'dark)
(sml/setup)
(set-face-attribute 'sml/modes nil :foreground "gray70")

;;; Outline

(with-eval-after-load 'outline
  (diminish 'outline-minor-mode))

;;; Ediff

(setq-default ediff-split-window-function 'split-window-horizontally)

;;; Eldoc

(ignore-errors (diminish 'eldoc-mode "doc"))
(setq-default eldoc-idle-delay 0)

;;; DocView

(with-eval-after-load 'doc-view
  (setq-default doc-view-continuous t
                doc-view-resolution 300))

;;; YASnippet

(with-eval-after-load 'yasnippet
  (diminish 'yas-minor-mode))

;;; ispell

(with-eval-after-load 'ispell
  (setq-default ispell-parser 'use-mode-name))

;;; Frame files

(add-to-list 'auto-mode-alist '("\\.frame\\'" . csharp-mode))

;;; All the rest

(cl-loop for file in (directory-files "~/.emacs.d/init/mode-specific/" t ".*\\.el\\'")
         do (load-init-file file))
