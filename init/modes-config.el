;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; IDO

(ido-mode)
(add-hook 'after-init-hook 'ido-ubiquitous-mode)

(setq-default ido-everywhere t
              ido-enable-flex-matching t
              ido-case-fold t
              ido-auto-merge-work-directories-length -1
              ido-create-new-buffer 'always)

;; Eww

(defun setup-eww ()
  (interactive)
  (setq-local show-trailing-whitespace nil))

(add-hook 'eww-mode-hook #'setup-eww)

;; Easy escape

(add-to-list 'load-path "~/.emacs.d/lisp/easy-escape")

;; Lisps

(defun setup-lisp ()
  (when (require 'easy-escape nil t)
    (easy-escape-minor-mode))
  (aggressive-indent-mode))

(add-hook 'lisp-mode-hook 'setup-lisp)
(add-hook 'emacs-lisp-mode-hook 'setup-lisp)

;; TRAMP

(when-os 'windows-nt
  (setq tramp-mode nil))

;; Custom mode for messages

(require 'eml-mode "~/.emacs.d/lisp/bits-n-pieces/eml-mode.el" t)

;; Prettify

(add-to-list 'load-path "~/.emacs.d/lisp/prettify-alists/")

;; Compilation

(setq-default compilation-scroll-output 'first-error)

;; Recentf

(add-hook 'after-init-hook 'recentf-mode)

;; multiple-cursors ;;

(with-eval-after-load 'multiple-cursors-core
  (define-key mc/keymap (kbd "C-,") 'mc/unmark-next-like-this)
  (define-key mc/keymap (kbd "C-.") 'mc/skip-to-next-like-this))

;; Powerline ;;

(setq sml/theme 'dark)
(sml/setup)
(set-face-attribute 'sml/modes nil :foreground "gray70")

;; Elapsed time (stopwatch)

(require 'elapsed "~/.emacs.d/lisp/elapsed/elapsed.el" t)

;; Outline

(with-eval-after-load 'outline
  (diminish 'outline-minor-mode))

;; Ediff ;;

(setq-default ediff-split-window-function 'split-window-horizontally)

;; Eldoc ;;

(ignore-errors (diminish 'eldoc-mode "doc"))
(setq-default eldoc-idle-delay 0)

;; Magit ;;

;; (setq magit-last-seen-setup-instructions "1.4.0")

(with-eval-after-load 'magit
  (setq-default magit-diff-refine-hunk 'all
                magit-push-always-verify nil
                magit-diff-arguments '("--minimal" "--ignore-all-space"))
  (let ((magit-faces '(magit-diff-added magit-diff-removed magit-diff-our magit-diff-base magit-diff-their
                                        magit-diff-context magit-diff-added-highlight magit-diff-removed-highlight
                                        magit-diff-our-highlight magit-diff-base-highlight magit-diff-their-highlight
                                        magit-diff-context-highlight)))
    (dolist (face magit-faces)
      (set-face-attribute face nil :foreground "#eeeeec")))
  (add-hook 'magit-mode-hook 'visual-line-mode))

;; FlyCheck ;;

(require 'flycheck "~/.emacs.d/lisp/flycheck/flycheck.el")

(with-eval-after-load 'flycheck
  ;; (diminish 'flycheck-mode "fc")
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)
                flycheck-display-errors-function #'flycheck-pos-tip-error-messages
                flycheck-checkers (cons 'python-pylint (remove 'python-pylint flycheck-checkers)))
  (add-to-list 'flycheck-locate-config-file-functions #'my-flycheck-locate-config-file))

;; DocView ;;

(with-eval-after-load 'doc-view
  (setq-default doc-view-continuous t
                doc-view-resolution 300))

;; Exapnd-region

;; From https://github.com/magnars/expand-region.el/issues/159
;; When popping the mark, continue popping until the cursor actually moves
;; Also, if the last command was a copy - skip past all the expand-region cruft.
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (when (eq last-command 'kill-ring-save)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

(defun company-yasnippet-or-completion ()
  (interactive)
  (let ((yas-fallback-behavior nil)
        (yas-enabled (and (featurep 'yasnippet)
                          (or (bound-and-true-p yas-mode)
                              (bound-and-true-p yas-minor-mode)))))
    (unless (and yas-enabled (yas-expand))
      (call-interactively #'company-complete-common))))

(defun setup-company ()
  (local-set-key [\C-return] 'company-manual-begin)
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  (substitute-key-definition 'company-complete-common 'company-yasnippet-or-completion company-active-map))

(with-eval-after-load 'company
  (setq-default company-idle-delay 0
                company-tooltip-align-annotations t
                company-dabbrev-code-everywhere t
                company-require-match "never")
  (setq-default company-quickhelp-delay 0
                company-quickhelp-max-lines 10) ;; Slows everything down
  (diminish 'company-mode))

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'company-mode-hook #'setup-company)

;; Elpy ;;

(with-eval-after-load 'elpy
  (put 'pyvenv-workon 'safe-local-variable #'stringp)

  (let ((elpy-disabled '(elpy-module-highlight-indentation elpy-module-flymake)))
    (setq-default elpy-rpc-backend "jedi"
                  elpy-modules (cl-loop for x in elpy-modules
                                        unless (memq x elpy-disabled)
                                        collect x)))

  (when-os 'windows-nt
    (setq-default python-shell-interpreter "pythonw"))
  (when-os 'gnu/linux
    (setq-default python-shell-interpreter "python3")))

(defun setup-elpy ()
  (set (make-local-variable 'flycheck-display-errors-function) #'my-python-display-errors)

  (require 'python-prettify)
  (setq-local prettify-symbols-alist python-prettify-symbols-alist)
  (prettify-symbols-mode)

  (let* ((buffer-path (buffer-file-name))
         (true-path   (and buffer-path (file-truename buffer-path))))
    (if (and true-path (string-match-p (regexp-opt '("afs")) true-path))
        (progn (elpy-mode -1)
               (eldoc-mode -1)
               (hs-minor-mode -1)
               (unload-feature 'elpy))
      (elpy-enable)
      (elpy-mode))))

(add-hook 'python-mode-hook #'setup-elpy)

;; AucTex ;;

(with-eval-after-load 'latex
  (add-to-list 'TeX-command-list '("Make" "make" TeX-run-compile nil t))

  (setq-default TeX-master t ;; Use current file as master; overwrite using .dir-locals.el if needed
                TeX-engine 'xetex
                reftex-plug-into-AUCTeX t
                LaTeX-verbatim-environments-local '("lstlisting")
                TeX-command-extra-options "-shell-escape"))

(defun LaTeX-wrap-in-math (start end)
  (interactive "r")
  (when (and (region-active-p) start end)
    (let ((math (buffer-substring start end)))
      (delete-region start end)
      (insert (concat "\\(" math "\\)")))))

(defun setup-auctex ()
  (interactive)

  (require 'greek-prettify)
  (setq prettify-symbols-alist prettify-symbols-greek-alist)
  (define-key LaTeX-mode-map (kbd "C-c w") #'LaTeX-wrap-in-math)

  (prettify-symbols-mode)
  (flyspell-mode)
  (flycheck-mode)
  (yas-minor-mode)
  (yas-reload-all)

  (TeX-source-correlate-mode)
  (company-auctex-init)
  (turn-on-reftex))

(add-to-list 'auto-mode-alist '("\\.tex$" . LaTeX-mode))
(add-hook 'LaTeX-mode-hook #'setup-auctex)

;; YASnippet

(with-eval-after-load 'yasnippet
  (diminish 'yas-minor-mode))

;; Org ;;

(with-eval-after-load 'org
  (setq-default org-log-done 'time
                org-support-shift-select t
                org-use-fast-todo-selection 'prefix
                org-todo-keywords '((sequence "TODO(t)" "IN PROGRESS(p)" "LATER(l)" "|" "DONE(d)"))))
;; (setq-local user-full-name "Clément Pit-\\kern0pt-Claudel") ;; Doesn't seem to work

(add-hook 'org-mode-hook #'flyspell-mode)

;; Markdown ;;

(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-c t") 'today)
  (define-key markdown-mode-map (kbd "C-c n") 'now)
  (setq-default markdown-command "pandoc --mathjax --standalone"))

(defun setup-markdown ()
  (flyspell-mode)
  (electric-indent-local-mode -1))

(add-hook 'markdown-mode-hook #'setup-markdown)
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

;; ispell ;;

(with-eval-after-load 'ispell
  (setq ispell-parser 'use-mode-name))

;; GPG files ;;

(defun setup-gpg-maybe ()
  (when (and buffer-file-name (string-match epa-file-name-regexp buffer-file-name))
    (message "Backup inhibited for this file")
    (setq-local backup-inhibited t)
    (auto-save-mode -1)))

(add-hook 'find-file-hook #'setup-gpg-maybe)

;; Coq and Proof-General ;;

(add-to-list 'load-path "~/.emacs.d/lisp/ProofGeneral/generic/")
(add-to-list 'load-path "~/.emacs.d/lisp/company-coq/")

(require 'proof-site nil t)

(setq-default proof-silence-compatibility-warning t
              proof-splash-enable nil
              proof-three-window-mode-policy 'hybrid)

(put #'company-coq-fold 'disabled nil)

(with-eval-after-load 'company-coq
  (setq-default company-coq-extra-symbols-cmd "SearchAbout -\"__\""
                company-coq-dynamic-autocompletion t
                company-coq-explicit-placeholders t
                company-coq-prettify-symbols t)
  (define-key company-coq-map (kbd "<f9>") #'prettify-symbols-mode))

(defun setup-coq ()
  (require 'company-coq)
  (diminish 'holes-mode)

  (when-os 'windows-nt
    (setq coq-prog-name "C:\\Coq\\bin\\coqtop.exe"))

  ;; (setq-default shr-use-fonts nil) ;; For presentation
  (require 'greek-prettify)
  (setq prettify-symbols-alist `((":=" . ?≜) ("Proof." . ?∵) ("::" . ?∷)
                                 ("Qed." . ?■) ("Defined." . ?□) ("Admitted." . ?⛐)
                                 ("Time" . ?⏱) ("Fail" . ?😱)
                                 ,@prettify-symbols-greek-alist)) ;;☢
  (company-coq-initialize))

(add-hook 'coq-mode-hook #'setup-coq)

(defconst coq-compilers-alist
  '((default . ("coqtop" . ("-emacs")))
    (coq-8.4pl2 . ("/build/coq-8.4pl2/bin/coqtop" . ("-emacs" "-coqlib" "/build/coq-8.4pl2/")))
    (coq-trunk . ("/build/coq-trunk-pr/bin/coqtop" . ("-emacs" "-coqlib" "/build/coq-trunk-pr/")))))

(require 'dash)

(defun coq-change-compiler (compiler-and-args)
  (interactive
   (let ((compiler-and-args (completing-read "Compiler: " coq-compilers-alist)))
     (list (or (alist-get (intern compiler-and-args) coq-compilers-alist) compiler-and-args))))
  (when (consp compiler-and-args)
    (progn
      (message "Compiler set to %s %s"
               (setq coq-prog-name (car compiler-and-args))
               (setq coq-prog-args (cdr compiler-and-args)))
      (when (functionp #'proof-shell-exit) (proof-shell-exit)))))

;; Agda

(require 'agda2 "~/.cabal/share/x86_64-linux-ghc-7.8.3/Agda-2.4.3/emacs-mode/agda2.el" t)

(defun setup-agda ()
  (require 'agda-prettify)
  (add-to-list 'agda2-include-dirs "/build/agda-stdlib/src/")
  (customize-set-variable 'agda2-highlight-face-groups 'default-faces)
  (setq prettify-symbols-alist agda-prettify-symbols-alist)
  (prettify-symbols-mode))

(add-hook 'agda2-mode-hook #'setup-agda)

;; Haskell

(with-eval-after-load 'haskell-mode
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))

(with-eval-after-load 'haskell-cabal
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))

(with-eval-after-load 'haskell-mode ;; From the manual
  (define-key haskell-mode-map (kbd "C-x C-d") nil)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
  (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c M-.") nil)
  (define-key haskell-mode-map (kbd "C-c C-d") nil))

(defun setup-haskell-prettify ()
  (require 'haskell-prettify)
  (setq prettify-symbols-alist haskell-prettify-symbols-alist)
  (prettify-symbols-mode))

(defun setup-haskell ()
  (setup-haskell-prettify)
  (haskell-indentation-mode)
  (add-hook 'completion-at-point-functions 'haskell-process-completions-at-point nil t))

(add-hook 'haskell-mode-hook #'setup-haskell)
(add-hook 'haskell-interactive-mode-hook #'setup-haskell-prettify)

;; C#

(defun setup-csharp ()
  (setq-local c-basic-offset 2)
  (require 'omnisharp "~/.emacs.d/lisp/omnisharp-emacs/omnisharp.el" t)
  (omnisharp-mode))

(with-eval-after-load 'csharp-mode
  (setq-default csharp-want-imenu nil)) ;; Horribly slow

(with-eval-after-load 'omnisharp
  (setq-default omnisharp-server-executable-path "/build/omnisharp-roslyn/scripts/Omnisharp"
                omnisharp-eldoc-support nil)
  (define-key omnisharp-mode-map (kbd "M-.") #'omnisharp-go-to-definition))

(add-hook 'csharp-mode-hook #'setup-csharp)

;; Dafny

(add-to-list 'load-path "~/.emacs.d/lisp/boogie-friends/emacs/")
(require 'dafny-mode nil t)
(require 'boogie-mode nil t)
(require 'z3-smt2-mode nil t)

(with-eval-after-load 'boogie-friends
  (setq-default boogie-prover-alternate-args '("/pretty:0"))
  (setq-default dafny-prover-alternate-args '("/pretty:0"))
  (when-os 'gnu/linux
    (setq-default flycheck-z3-smt2-executable "/build/MSR/z3/build/z3"
                  flycheck-dafny-executable "/build/MSR/dafny/Binaries/Dafny.exe"
                  flycheck-boogie-executable "/build/MSR/boogie/Binaries/Boogie.exe"
                  boogie-friends-profile-analyzer-executable "/build/MSR/vcc/vcc/Tools/Z3Visualizer/Z3Visualizer/bin/Debug/Z3AxiomProfiler.exe"))
  (when-os '(windows-nt cygwin)
    (setq-default flycheck-z3-smt2-executable "C:/MSR/dafny/Binaries/z3.exe"
                  flycheck-dafny-executable "C:/MSR/dafny/Binaries/Dafny.exe"
                  flycheck-boogie-executable "C:/MSR/boogie/Binaries/Boogie.exe"
                  boogie-friends-profile-analyzer-executable "C:/Program Files (x86)/Microsoft Research/Vcc/Binaries/Z3AxiomProfiler.exe")))

(defun setup-boogie-friends ()
  (diminish-undo 'flycheck-mode)
  (setq-local dafny-prover-background-args '("/printTooltips"))
  (setq-local flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(add-hook 'boogie-friends-hook #'setup-boogie-friends)
