;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'easy-escape "~/.emacs.d/lisp/easy-escape/easy-escape.el" t)
(require 'always-make-directory "~/.emacs.d/lisp/always-make-directory/always-make-directory.el" t)
(require 'presenter-mode "~/.emacs.d/lisp/presenter-mode/presenter-mode.el" t)
(require 'quick-peek "~/.emacs.d/lisp/quick-peek/quick-peek.el")
(require 'indirect-font-lock "~/.emacs.d/lisp/indirect-font-lock/indirect-font-lock.el")

(add-to-list 'load-path "~/.emacs.d/lisp/biblio.el/")
(require 'biblio)

;;; prettify
(add-to-list 'load-path "~/.emacs.d/lisp/prettify-alists/")
;; Made useless by show-key: (setq-default prettify-symbols-unprettify-at-point 'right-edge)

;;; diff
(setq-default diff-switches '("-u" "-Z"))
(add-hook 'diff-mode-hook #'hide-trailing-whitespace)

;; ASM

(setq-default asm-comment-char ?\#)

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
(setq-default ag-highlight-search t
              ag-group-matches nil)

;; ESH
(add-to-list 'load-path "~/.emacs.d/lisp/esh/")

;;; Spelling
(when-os 'windows-nt
  (setq-default ispell-program-name "c:/Program Files (x86)/Aspell/bin/aspell.exe"))

;;; Directory tracking
(setq-default dirtrack-list '("^\\[01;32m[0-9][0-9]:[0-9][0-9]:[0-9][0-9] \\[00;31m\\[01;35m\\[00;33m\\(.*\\)\n\\[01;34m\\$\\[00m" 1))
(add-hook 'shell-mode-hook #'dirtrack-mode)

;;; Compilation
(setq-default compilation-scroll-output 'first-error)
(with-eval-after-load 'compile
  (define-key compilation-shell-minor-mode-map (kbd "<f5>") #'recompile))

;;; Recentf
(add-hook 'after-init-hook 'recentf-mode)
(setq recentf-max-saved-items 1000)

;;; Occur and read-regexp
(setq-default read-regexp-defaults-function
              (lambda () (regexp-quote (symbol-name (symbol-at-point)))))

;;; TiML
;; (load "/build/lambda-c/emacs/timl")
;; (add-to-list 'auto-mode-alist '("\\.timl\\'" . timl-mode))

;;; Which key
(trycall #'which-key-mode)

;;; Alert
(setq-default alert-default-style 'libnotify)

;;; Nameless
(setq ;; nameless-global-aliases '(("fl:" . "font-lock"))
 nameless-private-prefix t)

;;; Modeline info
(setq-default display-time-load-average-threshold 1)
(display-time-mode)
(display-battery-mode)

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
  (add-to-list 'sml/replacer-regexp-list '("^~/\\.emacs\\.d/.cask/" ":CASK:"))
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
(setq-default dired-listing-switches "-alh"
              dired-dwim-target t)
(require 'dired-x) ;; C-x C-j

;; Wgrep
(setq-default wgrep-auto-save-buffer t)

;; reStructuredText
(with-eval-after-load 'rst
  (define-key rst-mode-map (kbd "<f12>") #'~/rst-coq-action)
  (add-hook 'rst-mode-hook #'flycheck-mode))

;; Checkdoc

(setq-default checkdoc-arguments-in-order-flag nil
              checkdoc-verb-check-experimental-flag nil)

;;; All the rest

(defconst ~/init-mode-specific-dir (expand-file-name "mode-specific/" ~/init-dir))
(mapc #'~/load-file (directory-files ~/init-mode-specific-dir t ".*\\.el\\'"))
