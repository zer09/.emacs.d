;; Coq and Proof-General

(add-to-list 'load-path "~/.emacs.d/lisp/ProofGeneral/generic/")
(add-to-list 'load-path "~/.emacs.d/lisp/company-coq/")
(add-to-list 'load-path "~/.emacs.d/lisp/company-coq/experiments/")

(require 'proof-site nil t)

(setq-default proof-silence-compatibility-warning t
              proof-splash-enable nil
              proof-three-window-mode-policy 'hybrid
              coq-compile-parallel-in-background t
              coq-pre-v85 t)

(when-os 'windows-nt
  (setq coq-prog-name "C:\\Coq\\bin\\coqtop.exe"))

(put #'company-coq-fold 'disabled nil)

(with-eval-after-load 'company-coq
  (setq-default company-coq-extra-symbols-cmd "SearchAbout -\"__\""
                company-coq-disabled-features nil
                company-coq--prettify-abbrevs t)
  (define-key company-coq-map (kbd "<f10>") #'coq-compile-before-require-toggle)
  (define-key company-coq-map (kbd "<f9>") #'prettify-symbols-mode)
  (define-key company-coq-map (kbd "C-c C-j") #'company-coq-proof-goto-point))

(defun setup-coq ()
  (diminish 'holes-mode)

  (require 'cpc-alerts)
  (cpc-alerts-mode)

  (when (not (display-graphic-p))
    (set-face-attribute 'proof-locked-face nil
                        :background "darkslateblue" :underline nil))

  ;; (setq-default shr-use-fonts nil) ;; For presentation
  (require 'greek-prettify)
  (setq prettify-symbols-alist `((":=" . ?≜) ("Proof." . ?∵) ("::" . ?∷) ;≔
                                 ("Qed." . ?■) ("Defined." . ?□) ("Admitted." . ?😱)
                                 ("Time" . ?⏱) ("Fail" . ?⛐)
                                 ,@prettify-symbols-greek-alist)) ;;☢
  (require 'company-coq)
  (company-coq-mode))

;; (company-coq-initialize))

(add-hook 'coq-mode-hook #'setup-coq)

;; Instead of specifying -coqlib, tell Coq's ./configure where to install the
;; files, and it will hardocde the paths in the binary. Use './env' in the source
;; folder for now.

(defconst coq-compilers-alist
  '((default . ("coqtop" . ("-emacs")))
    (coq-8.4pl2 . ("/build/coq-8.4pl2/bin/coqtop" . ("-emacs" "-coqlib" "/build/coq-8.4pl2/")))
    (profiler-8.4pl4 . ("/build/coq-8.4-profiler/bin/coqtop" . ("-emacs" "-coqlib" "/build/coq-8.4-profiler/")))
    (coq-8.5 . ("/build/coq-8.5/env/bin/coqtop" . ("-emacs")))
    (letouzey . ("/build/coq-wip-letouzey/env/bin/coqtop" . ("-emacs")))))

(defun coq-change-compiler (compiler-and-args)
  "Change Coq compiler to COMPILER-AND-ARGS."
  (interactive
   (let ((compiler-and-args (completing-read "Compiler: " coq-compilers-alist)))
     (list (alist-get (intern compiler-and-args) coq-compilers-alist))))
  (when (consp compiler-and-args)
    (progn
      (message "Compiler set to %s %s"
               (setq coq-prog-name (car compiler-and-args))
               (setq coq-prog-args (cdr compiler-and-args)))
      (when (functionp #'proof-shell-exit) (proof-shell-exit)))))
