;; Coq and Proof-General

(add-to-list 'load-path "~/.emacs.d/lisp/ProofGeneral/generic/")
(add-to-list 'load-path "~/.emacs.d/lisp/company-coq/")

(require 'proof-site nil t)

(setq-default proof-silence-compatibility-warning t
              proof-splash-enable nil
              proof-three-window-mode-policy 'hybrid)

(when-os 'windows-nt
  (setq coq-prog-name "C:\\Coq\\bin\\coqtop.exe"))

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
