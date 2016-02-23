;; Coq and Proof-General

(add-to-list 'load-path "~/.emacs.d/lisp/company-coq/")
(add-to-list 'load-path "~/.emacs.d/lisp/company-coq/experiments/")
(add-to-list 'load-path "~/.emacs.d/lisp/company-coq/experiments/company-coq-term-builder/")
(add-to-list 'load-path "~/.emacs.d/lisp/company-coq/experiments/company-coq-LaTeX/")

(load-file "/home/clement/documents/mit/frap/frap.el")
(require 'proof-site "~/.emacs.d/lisp/ProofGeneral/generic/proof-site")

(require 'presenter-mode nil t)

(setq-default proof-silence-compatibility-warning t
              proof-splash-enable nil
              proof-three-window-mode-policy 'hybrid
              coq-compile-parallel-in-background t
              coq-pre-v85 t)

(when-os 'windows-nt
  (setq coq-prog-name "C:\\Coq\\bin\\coqtop.exe"))

(put #'company-coq-fold 'disabled nil)

(with-eval-after-load 'company-coq
  (setq-default company-coq-live-on-the-edge t
                company-coq-dynamic-autocompletion t
                company-coq-initial-fold-state 'bullets
                company-coq-extra-symbols-cmd "SearchAbout -\"__\""
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
  (setq prettify-symbols-alist `((":=" . ?≜) ("Proof." . ?∵) ("::" . ?∷) ; ≔
                                 ("Qed." . ?■) ("Defined." . ?□) ;; ("Admitted." . ?😱) ("Fail" . ?⛐)
                                 ("Time" . ?⏱)
                                 ,@prettify-symbols-greek-alist)) ;;☢
  (require 'company-coq)
  (company-coq-mode)
  (load "company-coq-goal-diffs.el"))

(add-hook 'coq-mode-hook #'setup-coq)

;; Instead of specifying -coqlib, tell Coq's ./configure where to install the
;; files, and it will hardocde the paths in the binary. Use './dist' in the source
;; folder for now.

(defconst coq-compilers-alist
  '((default . "coqtop")
    (coq-8.5 . "/build/coq-8.5/dist/bin/coqtop")
    (coq-8.4pl2 . "/build/coq-8.4pl2/dist/bin/coqtop")
    (coq-8.5-patched . "/build/coq-8.5-patched/dist/bin/coqtop")
    (profiler-8.4pl4 . "/build/coq-8.4-profiler/dist/bin/coqtop")
    (letouzey . "/build/coq-wip-letouzey/dist/bin/coqtop")))

(defun coq-change-compiler (compiler)
  "Change Coq compiler to COMPILER-AND-ARGS."
  (interactive
   (let ((compiler (completing-read "Compiler: " coq-compilers-alist)))
     (list (cdr (assq (intern compiler) coq-compilers-alist)))))
  (when (stringp compiler)
    (progn
      (message "Compiler set to %s." (setq coq-prog-name compiler))
      (proof-shell-exit))))
