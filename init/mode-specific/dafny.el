(add-to-list 'load-path "~/.emacs.d/lisp/boogie-friends/emacs/")

(require 'dafny-mode nil t)
(require 'boogie-mode nil t)
(require 'z3-smt2-mode nil t)

(with-eval-after-load 'boogie-friends
  (let (root z3-exe)
    (when-os 'gnu/linux           (setq root "/build/MSR/" z3-exe "z3"))
    (when-os '(windows-nt cygwin) (setq root "C:/MSR/" z3-exe "z3.exe"))
    (setq-default dafny-verification-backend 'server
                  ;; inferior-dafny--write-transcript t
                  boogie-prover-alternate-args '("/pretty:0")
                  dafny-prover-alternate-args '("/pretty:0")
                  flycheck-z3-smt2-executable (expand-file-name z3-exe (expand-file-name "z3/bin" root))
                  flycheck-dafny-executable (expand-file-name "dafny/Binaries/Dafny.exe" root)
                  flycheck-boogie-executable (expand-file-name "boogie/Binaries/Boogie.exe" root)
                  flycheck-inferior-dafny-executable (expand-file-name "dafny/Binaries/DafnyServer.exe" root)
                  boogie-friends-profile-analyzer-executable "C:/MSR/Z3Visualizer/Z3Visualizer/bin/Debug/Z3AxiomProfiler.exe")))

(defun setup-boogie-friends ()
  (diminish-undo 'flycheck-mode)
  (add-to-list 'dafny-prover-background-args "/printTooltips")
  (setq-local flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(add-hook 'boogie-friends-hook #'setup-boogie-friends)
