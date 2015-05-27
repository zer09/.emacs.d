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

;; Company

(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company (diminish 'company-mode))

;; Recentf

(add-hook 'after-init-hook 'recentf-mode)

;; Powerline ;;

(add-to-list 'load-path "~/.emacs.d/lisp/emacs-powerline")
(require 'powerline nil t)

;; Powerline ;;

(add-to-list 'load-path "~/.emacs.d/lisp/own/elapsed")

;; Elapsed time (stowatch)

(require 'elapsed nil t)

;; Outline

(with-eval-after-load 'outline (diminish 'outline-minor-mode))

;; Ediff ;;

(setq-default ediff-split-window-function 'split-window-horizontally)

;; Eldoc ;;

(diminish 'eldoc-mode "doc")
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

;; Company ;;

;; (add-to-list 'load-path "~/.emacs.d/lisp/company-mode/")
;; (require 'company)

(defun company-yasnippet-or-completion ()
  (interactive)
  (let ((yas-fallback-behavior nil)
        (yas-enabled (and (featurep 'yasnippet)
                          (or (bound-and-true-p yas-mode)
                              (bound-and-true-p yas-minor-mode)))))
    (unless (and yas-enabled (yas-expand))
      (call-interactively #'company-complete-common))))

(defun setup-company ()
  (interactive)

  (setq-default company-idle-delay 0
                company-tooltip-align-annotations t
                company-dabbrev-code-everywhere t
                company-require-match "never")
  (setq-default company-quickhelp-delay 0
                company-quickhelp-max-lines 10) ;; Slows everything down

  (local-set-key [\C-return] 'company-manual-begin)
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  (substitute-key-definition 'company-complete-common 'company-yasnippet-or-completion company-active-map))

(add-hook 'company-mode-hook #'setup-company)

;; HTML
(defun html-setup ()
  (interactive)
  (add-to-list 'load-path "~/.emacs.d/lisp/own/company-html/")
  (require 'company-html)
  (add-to-list 'company-backends #'company-html-tags))

(add-hook 'html-mode-hook #'html-setup)

;; Elpy ;;

(defun elpy-if-not-afs ()
  (interactive)
  (require 'elpy)
  (put 'pyvenv-workon 'safe-local-variable #'stringp)

  (setq-default tab-width 4
                indent-tabs-mode nil
                python-shell-interpreter "python3"
                python-check-command "/usr/local/bin/epylint")

  (let* ((buffer-path (buffer-file-name))
         (true-path   (and buffer-path (file-truename buffer-path))))
    (if (and true-path (string-match-p (regexp-opt '("afs")) true-path))
        (progn (elpy-mode -1)
               (eldoc-mode -1)
               (hs-minor-mode -1)
               (unload-feature 'elpy))
      (elpy-enable)
      (flycheck-mode -1) ;; elpy takes care of this
      (setq-default elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules)
                    elpy-rpc-backend "jedi"
                    elpy-rpc-python-command "python3")
      (elpy-mode))))

(add-hook 'python-mode-hook #'elpy-if-not-afs)

;; AucTex ;;

(defun setup-auctex ()
  (interactive)

  (add-to-list 'TeX-command-list '("Make" "make" TeX-run-compile nil t))

  ;; (let ((prettify-alist (cl-loop for (k . v) in prettify-symbols-greek-alist
                                 ;; collect (cons (concat "\\" k) v))))
  (setq-default TeX-master t ;; Use current file as master; overwrite using .dir-locals.el if needed
                TeX-engine 'xetex
                reftex-plug-into-AUCTeX t
                LaTeX-verbatim-environments-local '("lstlisting")
                TeX-command-extra-options "-shell-escape"
                prettify-symbols-alist prettify-symbols-greek-alist)

  (prettify-symbols-mode 1)
  (flyspell-mode 1)
  (flycheck-mode 1)
  (yas-minor-mode 1)
  (yas-reload-all)

  (TeX-source-correlate-mode 1)
  (company-auctex-init)
  (turn-on-reftex))

(add-to-list 'auto-mode-alist '("\\.tex$" . LaTeX-mode))
(add-hook 'LaTeX-mode-hook #'setup-auctex)

;; YASnippet

(with-eval-after-load 'yasnippet (diminish 'yas-minor-mode))

;; Org ;;

(defun setup-org ()
  (flyspell-mode)
  (setq-default org-log-done t
                org-support-shift-select 'always
                org-todo-keywords '((sequence "TODO(t)" "LATER(l)" "|" "DONE(d)"))))
;; (setq-local user-full-name "Clément F Pit-\\kern0pt-Claudel") ;; Doesn't seem to work

(add-hook 'org-mode-hook #'setup-org)

;; Markdown ;;

(defun setup-markdown ()
  (flyspell-mode)
  (define-key markdown-mode-map (kbd "C-c t") 'today)
  (define-key markdown-mode-map (kbd "C-c n") 'now)
  (electric-indent-local-mode -1)
  (setq-default markdown-command "pandoc --mathjax --standalone"))

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

(defun setup-coq ()
  (require 'company-coq)
  (diminish 'holes-mode)
  ;; (setq-default shr-use-fonts nil) ;; For presentation
  (setq-default company-coq-extra-symbols-cmd "SearchAbout -\"__\""
                company-coq-dynamic-autocompletion t
                company-coq-explicit-placeholders t
                company-coq-prettify-symbols t
                prettify-symbols-alist `((":=" . ?≜) ("Proof." . ?∵)
                                         ("Qed." . ?■) ("Defined." . ?□)
                                         ("Admitted." . ?😱) ("Time" . ?⏱) ("Fail" . ?⛐)
                                         ,@prettify-symbols-greek-alist)) ;;☢
  (define-key company-coq-map (kbd "<f9>") #'prettify-symbols-mode)
  (company-coq-initialize))

(add-hook 'coq-mode-hook #'setup-coq)

;; Agda

(add-to-list 'load-path "/home/clement/.cabal/share/x86_64-linux-ghc-7.8.3/Agda-2.4.3/emacs-mode/")
(require 'agda2 nil t)
