;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; IDO ;;

(require 'ido)

(ido-mode t)
(add-hook 'after-init-hook 'ido-ubiquitous-mode)

(setq-default ido-everywhere t
              ido-enable-flex-matching t
              ido-case-fold t
              ido-auto-merge-work-directories-length -1
              ido-create-new-buffer 'always)

;; Easy escape

(add-to-list 'load-path "~/.emacs.d/lisp/own/easy-escape")

(when (require 'easy-escape nil t)
  (add-hook 'prog-mode-hook 'easy-escape-minor-mode))

;; TRAMP

(when-os 'windows
  (setq tramp-mode nil))

;; Prettify

(defconst prettify-symbols-greek-alist '(("Alpha" . ?Α) ("Beta" . ?Β) ("Gamma" . ?Γ)
                                         ("Delta" . ?Δ) ("Epsilon" . ?Ε) ("Zeta" . ?Ζ)
                                         ("Eta" . ?Η) ("Theta" . ?Θ) ("Iota" . ?Ι)
                                         ("Kappa" . ?Κ) ("Lambda" . ?Λ) ("Mu" . ?Μ)
                                         ("Nu" . ?Ν) ("Xi" . ?Ξ) ("Omicron" . ?Ο)
                                         ("Pi" . ?Π) ("Rho" . ?Ρ) ("Sigma" . ?Σ)
                                         ("Tau" . ?Τ) ("Upsilon" . ?Υ) ("Phi" . ?Φ)
                                         ("Chi" . ?Χ) ("Psi" . ?Ψ) ("Omega" . ?Ω)
                                         ("alpha" . ?α) ("beta" . ?β) ("gamma" . ?γ)
                                         ("delta" . ?δ) ("epsilon" . ?ε) ("zeta" . ?ζ)
                                         ("eta" . ?η) ("theta" . ?θ) ("iota" . ?ι)
                                         ("kappa" . ?κ) ("lambda" . ?λ) ("mu" . ?μ)
                                         ("nu" . ?ν) ("xi" . ?ξ) ("omicron" . ?ο)
                                         ("pi" . ?π) ("rho" . ?ρ) ("sigma" . ?σ)
                                         ("tau" . ?τ) ("upsilon" . ?υ) ("phi" . ?φ)
                                         ("chi" . ?χ) ("psi" . ?ψ) ("omega" . ?ω)
                                         ("varepsilon" . ?ε) ("varkappa" . ?ϰ)
                                         ("varphi" . ?φ) ("varpi" . ?ϖ)
                                         ("varsigma" . ?ς)))

;; Compilation

(setq-default compilation-scroll-output 'first-error)

;; Recentf

(add-hook 'after-init-hook 'recentf-mode)

;; Powerline ;;

(add-to-list 'load-path "~/.emacs.d/lisp/emacs-powerline")
(require 'powerline nil t)

;; Elapsed time (stopwatch)

(add-to-list 'load-path "~/.emacs.d/lisp/own/elapsed")
(require 'elapsed nil t)

;; Outline

(with-eval-after-load 'outline
  (diminish 'outline-minor-mode))

;; Ediff ;;

(setq-default ediff-split-window-function 'split-window-horizontally)

;; Eldoc ;;

(ignore-errors (diminish 'eldoc-mode "doc"))
(setq-default eldoc-idle-delay 0)

;; Magit ;;

(setq magit-last-seen-setup-instructions "1.4.0")

(with-eval-after-load 'magit
  (setq-default magit-diff-refine-hunk 'all
                magit-diff-options '("--minimal" "--ignore-all-space"))
  (add-hook 'magit-mode-hook 'visual-line-mode))

;; FlyCheck ;;

(with-eval-after-load 'flycheck
  (diminish 'flycheck-mode "fc")
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; Insert accented characters
;; (load-library "iso-transl")

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

  (setq-default elpy-rpc-backend "jedi"
                elpy-modules (dolist (mod '(elpy-module-highlight-indentation elpy-module-flymake))
                               (setq elpy-modules (delq mod elpy-modules))))

  (let ((pylint-rc (concat "--rcfile=" (expand-file-name "~/.emacs.d/external-config/.pylintrc"))))
    (when-os 'windows
      (setq-default python-shell-interpreter "pythonw"
                    python-check-command (concat "pylint " pylint-rc)))
    (when-os 'gnu/linux
      (setq-default python-shell-interpreter "python3"
                    python-check-command (concat "epylint " pylint-rc))))

  (set (make-local-variable 'flycheck-display-errors-function) #'my-python-display-errors))

(defun elpy-if-not-afs ()
  (let* ((buffer-path (buffer-file-name))
         (true-path   (and buffer-path (file-truename buffer-path))))
    (if (and true-path (string-match-p (regexp-opt '("afs")) true-path))
        (progn (elpy-mode -1)
               (eldoc-mode -1)
               (hs-minor-mode -1)
               (unload-feature 'elpy))
      (elpy-enable)
      (elpy-mode))))

(add-hook 'python-mode-hook #'elpy-if-not-afs)

;; AucTex ;;

(with-eval-after-load 'auctex
  (add-to-list 'TeX-command-list '("Make" "make" TeX-run-compile nil t))

  (setq-default TeX-master t ;; Use current file as master; overwrite using .dir-locals.el if needed
                TeX-engine 'xetex
                reftex-plug-into-AUCTeX t
                LaTeX-verbatim-environments-local '("lstlisting")
                TeX-command-extra-options "-shell-escape"))

(defun setup-auctex ()
  (interactive)

  (setq prettify-symbols-alist prettify-symbols-greek-alist)

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
  (setq-default org-log-done 'note
                org-support-shift-select 'always
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

;; Coq and Proof-General ;;

(add-to-list 'load-path "~/.emacs.d/lisp/ProofGeneral/generic/")
(add-to-list 'load-path "~/.emacs.d/lisp/own/company-coq/")

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
  ;; (setq-default shr-use-fonts nil) ;; For presentation
  (setq prettify-symbols-alist `((":=" . ?≜) ("Proof." . ?∵)
                                 ("Qed." . ?■) ("Defined." . ?□)
                                 ("Admitted." . ?😱) ("Time" . ?⏱) ("Fail" . ?⛐)
                                 ,@prettify-symbols-greek-alist)) ;;☢
  (company-coq-initialize))

(add-hook 'coq-mode-hook #'setup-coq)

;; Agda

(add-to-list 'load-path "/home/clement/.cabal/share/x86_64-linux-ghc-7.8.3/Agda-2.4.3/emacs-mode/")
(require 'agda2 nil t)

;; Dafny

(add-to-list 'load-path "/home/clement/.emacs.d/lisp/own/boogie-friends/emacs/")
(require 'dafny-mode nil t)
(require 'boogie-mode nil t)

(with-eval-after-load 'boogie-friends
  (setq boogie-prover-alternate-args '("/proverLog:input.smt2"))
  (when-os 'gnu/linux
    (setq flycheck-z3-smt2-executable "/build/MSR/z3/build/z3")
    (setq flycheck-dafny-executable "/build/MSR/dafny/Binaries/Dafny.exe")
    (setq flycheck-boogie-executable "/build/MSR/boogie/Binaries/Boogie.exe"))
  (when-os 'windows
    (setq flycheck-z3-smt2-executable "C;/MSR/dafny/Binaries/z3.exe")
    (setq flycheck-dafny-executable "C:/MSR/dafny/Binaries/Dafny.exe")
    (setq flycheck-boogie-executable "C:/MSR/boogie/Binaries/Boogie.exe")))

(defun setup-boogie-friends ()
  (diminish-undo 'flycheck-mode))

(add-hook 'boogie-friends-hook #'setup-boogie-friends)
