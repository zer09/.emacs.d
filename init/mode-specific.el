;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'easy-escape "~/.emacs.d/lisp/easy-escape/easy-escape.el" t)
(require 'always-make-directory "~/.emacs.d/lisp/always-make-directory/always-make-directory.el" t)

;;; prettify
(add-to-list 'load-path "~/.emacs.d/lisp/prettify-alists/")
;; Made useless by show-key: (setq-default prettify-symbols-unprettify-at-point 'right-edge)

;;; diff
(setq-default diff-switches '("-u" "-Z"))
(add-hook 'diff-mode-hook #'hide-trailing-whitespace)

;;; comint

(add-hook 'comint-mode-hook #'hide-trailing-whitespace)

;;; eww

(setq-default network-security-level 'high)
(add-hook 'eww-mode-hook #'hide-trailing-whitespace)

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

(add-hook 'tuareg-mode-hook #'tuareg-breaks-which-function-mode)

;;; Recentf
(add-hook 'after-init-hook 'recentf-mode)
(setq recentf-max-saved-items 1000)

;;; Occur and read-regexp
(setq-default read-regexp-defaults-function
              (lambda () (regexp-quote (symbol-name (symbol-at-point)))))

;;; Which key
(trycall #'which-key-mode)

;; Alert
(setq-default alert-default-style 'libnotify)

;;; Keyfreq

(setq-default keyfreq-file "~/.emacs.d/keyfreq"
              keyfreq-file-lock "~/.emacs.d/keyfreq.lock")
(trycall #'keyfreq-mode 1)
(trycall #'keyfreq-autosave-mode 1)

;;; Page breaks
(global-page-break-lines-mode)

;;; smart-mode-line

(setq-default sml/theme 'dark
              sml/name-width '(10 . 40)
              sml/replacer-regexp-list (or (bound-and-true-p sml/replacer-regexp-list) nil))

(with-eval-after-load 'smart-mode-line
  (add-to-list 'sml/replacer-regexp-list '("^~/\\.emacs\\.d/lisp/" ":LISP:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/\\.emacs\\.d/init/" ":INIT:"))
  (set-face-attribute 'sml/modes nil :foreground "gray70"))
(sml/setup)

;;; Ediff

(setq-default ediff-split-window-function 'split-window-horizontally
              ediff-window-setup-function 'ediff-setup-windows-plain)

;;; Eldoc

(with-eval-after-load 'eldoc
  (diminish 'eldoc-mode " 📚 "))

(setq-default eldoc-idle-delay 0)

;;; DocView

(with-eval-after-load 'doc-view
  (setq-default doc-view-continuous t
                doc-view-resolution 300))

;;; ispell

(with-eval-after-load 'ispell
  (setq-default ispell-parser 'use-mode-name))

;;; Dired
(setq-default dired-listing-switches "-alh")
(require 'dired-x)

;;; Frame files

(add-to-list 'auto-mode-alist '("\\.frame\\'" . csharp-mode))

;;; All the rest

(defconst ~/init-mode-specific-dir (expand-file-name "mode-specific/" ~/init-dir))
(mapc #'~/load-file (directory-files ~/init-mode-specific-dir t ".*\\.el\\'"))
