;; Coq and Proof-General

(add-to-list 'load-path "~/.emacs.d/lisp/company-coq/")
(add-to-list 'load-path "~/.emacs.d/lisp/company-coq/experiments/")
(add-to-list 'load-path "~/.emacs.d/lisp/company-coq/experiments/company-coq-term-builder/")
(add-to-list 'load-path "~/.emacs.d/lisp/company-coq/experiments/company-coq-LaTeX/")

(ignore-errors (load-file "/home/clement/documents/mit/frap/frap.el"))
(require 'proof-site "~/.emacs.d/lisp/ProofGeneral/generic/proof-site" t)

(require 'presenter-mode nil t)

(setq-default proof-silence-compatibility-warning t
              proof-splash-enable nil
              proof-three-window-mode-policy 'hybrid
              coq-compile-parallel-in-background t)

(when-os 'windows-nt
  (setq coq-prog-name "C:\\Coq\\bin\\coqtop.exe"))

(put #'company-coq-fold 'disabled nil)

(with-eval-after-load 'company-coq
  (setq-default company-coq-live-on-the-edge t
                company-coq-dynamic-autocompletion t
                company-coq-initial-fold-state 'bullets
                company-coq-extra-symbols-cmd "SearchAbout -\"____\""
                company-coq-features/prettify-symbols-in-terminal t)
  (define-key company-coq-map (kbd "<f9>") #'prettify-symbols-mode)
  (define-key company-coq-map (kbd "<f10>") #'coq-compile-before-require-toggle)
  (define-key company-coq-map (kbd "C-c RET") #'company-coq-proof-goto-point)
  (define-key company-coq-map (kbd "C-c C-j") #'company-coq-proof-goto-point))

(defun setup-coq ()
  (when (not (display-graphic-p))
    (set-face-attribute 'proof-locked-face nil
                        :background "darkslateblue" :underline nil))

  ;; (setq-default shr-use-fonts nil) ;; For presentation
  (require 'greek-prettify)
  (setq prettify-symbols-alist `((":=" . ?≜) ("Proof" . ?∵) ("::" . ?∷) ; ≔
                                 ("Qed" . ?■) ("Defined" . ?□) ;; ("Admitted" . ?😱) ("Fail" . ?⛐)
                                 ("Time" . ?⏱)
                                 ,@prettify-symbols-greek-alist)) ;;☢
  (require 'company-coq)
  (company-coq-mode)
  (load "company-coq-goal-diffs.el"))

(add-hook 'coq-mode-hook #'setup-coq)

;; Instead of specifying -coqlib, tell Coq's ./configure where to install the
;; files, and it will hardocde the paths in the binary. Use './dist' in the source
;; folder for now.

(defconst coq-bin-dirs-alist
  '(("default" . "")
    ("coq-8.5" . "/build/coq-8.5/dist/bin/")
    ("coq-8.4pl2" . "/build/coq-8.4pl2/dist/bin/")
    ("coq-8.4pl6-patched" . "/build/coq-8.4pl6/dist/bin/")
    ("coq-8.4pl6-profiler" . "/build/coq-8.4pl6-profiler/dist/bin/")
    ("coq-8.5-patched" . "/build/coq-8.5-patched/dist/bin/")
    ("profiler-8.4pl4" . "/build/coq-8.4-profiler/dist/bin/")
    ("letouzey" . "/build/coq-wip-letouzey/dist/bin/")))

(defun coq-change-compiler (coq-bin-dir)
  "Change Coq executables to use those in COQ-BIN-DIR."
  (interactive
   (list (cdr (assq (completing-read "Compiler: " coq-bin-dirs-alist)
                    coq-bin-dirs-alist))))
  (when (stringp coq-bin-dir)
    (setq coq-compiler (concat coq-bin-dir "coqc"))
    (setq coq-prog-name (concat coq-bin-dir "coqtop"))
    (setq coq-dependency-analyzer (concat coq-bin-dir "coqdep"))
    (message "Using Coq binaries from %s." coq-bin-dir)
    (proof-shell-exit)))
