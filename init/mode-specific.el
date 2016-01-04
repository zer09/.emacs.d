;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'easy-escape "~/.emacs.d/lisp/easy-escape/easy-escape.el" t)
(require 'always-make-directory "~/.emacs.d/lisp/always-make-directory/always-make-directory.el" t)

;;; prettify
(add-to-list 'load-path "~/.emacs.d/lisp/prettify-alists/")
(setq-default prettify-symbols-unprettify-at-point 'right-edge)

;; diff
(setq-default diff-switches '("-u" "-Z"))

;;; TRAMP

(when-os 'windows-nt
  (setq tramp-mode nil))

;; Presentations
(require 'demo-mode  "~/.emacs.d/lisp/demo-mode/demo-mode.el" t)

;;; Email
(require 'eml-mode "~/.emacs.d/lisp/bits-n-pieces/eml-mode.el" t)

;;; Elapsed time (stopwatch)
(require 'elapsed "~/.emacs.d/lisp/elapsed/elapsed.el" t)

;;; ag
(setq-default ag-highlight-search t)

;;; Spelling
(when-os 'windows-nt
  (setq-default ispell-program-name "c:/Program Files (x86)/Aspell/bin/aspell.exe"))

;;; Compilation
(setq-default compilation-scroll-output 'first-error)

;;; OCAML
(defun tuareg-breaks-which-function-mode ()
  (which-function-mode -1))

(add-hook 'tuareg-mode-hook (tuareg-breaks-which-function-mode))

;;; Recentf
(add-hook 'after-init-hook 'recentf-mode)
(setq recentf-max-saved-items 100)

;;; Which key
(which-key-mode)
(diminish 'which-key-mode)

;;; Page breaks
(trycall global-page-break-lines-mode)
(optionally (diminish 'page-break-lines-mode))

;;; Powerline

(setq sml/theme 'dark)
(trycall sml/setup)
(optionally (set-face-attribute 'sml/modes nil :foreground "gray70"))

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

(defconst init-mode-specific-dir (expand-file-name "mode-specific/" init-dir))
(mapc #'init-load-file (directory-files init-mode-specific-dir t ".*\\.el\\'"))
