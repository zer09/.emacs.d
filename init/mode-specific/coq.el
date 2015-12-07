;; Coq and Proof-General

(add-to-list 'load-path "~/.emacs.d/lisp/ProofGeneral/generic/")
(add-to-list 'load-path "~/.emacs.d/lisp/company-coq/")

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
                company-coq-dynamic-autocompletion t
                company-coq-explicit-placeholders t
                company-coq-prettify-symbols t)
  (define-key company-coq-map (kbd "<f9>") #'prettify-symbols-mode)
  (define-key company-coq-map (kbd "<f10>") #'coq-compile-before-require-toggle))

(defun setup-coq ()
  (diminish 'holes-mode)

  (require 'cpc-alerts)
  (cpc-alerts-mode)

  ;; (setq-default shr-use-fonts nil) ;; For presentation
  (require 'greek-prettify)
  (setq prettify-symbols-alist `((":=" . ?≜) ("Proof." . ?∵) ("::" . ?∷) ;≔
                                 ("Qed." . ?■) ("Defined." . ?□) ("Admitted." . ?😱)
                                 ("Time" . ?⏱) ("Fail" . ?⛐)
                                 ,@prettify-symbols-greek-alist)) ;;☢

  (require 'company-coq)
  (company-coq-initialize))

(add-hook 'coq-mode-hook #'setup-coq)

(defconst coq-compilers-alist
  '((default . ("coqtop" . ("-emacs")))
    (coq-8.4pl2 . ("/build/coq-8.4pl2/bin/coqtop" . ("-emacs" "-coqlib" "/build/coq-8.4pl2/")))
    (profiler-8.4pl4 . ("/build/coq-8.4-profiler/bin/coqtop" . ("-emacs" "-coqlib" "/build/coq-8.4-profiler/")))
    (coq-trunk . ("/build/coq-trunk-pr/bin/coqtop" . ("-emacs" "-coqlib" "/build/coq-trunk-pr/")))))

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
