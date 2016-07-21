(add-to-list 'load-path "~/.emacs.d/lisp/boogie-friends/emacs/")

(with-eval-after-load 'boogie-friends
  (let (root z3-exe)
    (when-os 'gnu/linux           (setq root "/build/MSR/" z3-exe "z3"))
    (when-os '(windows-nt cygwin) (setq root "C:/MSR/" z3-exe "z3.exe"))
    (setq-default dafny-verification-backend 'server
                  ;; inferior-dafny--write-transcript t
                  boogie-prover-alternate-args '("/pretty:0")
                  dafny-prover-alternate-args '("/pretty:0")
                  flycheck-z3-smt2-executable "z3"
                  ;; (expand-file-name z3-exe (expand-file-name "dafny/Binaries/z3/bin" root))
                  flycheck-dafny-executable
                  (expand-file-name "dafny/Binaries/Dafny.exe" root)
                  flycheck-boogie-executable
                  (expand-file-name "boogie/Binaries/Boogie.exe" root)
                  flycheck-inferior-dafny-executable
                  (expand-file-name "dafny/Binaries/DafnyServer.exe" root)
                  boogie-friends-profile-analyzer-executable
                  (expand-file-name "Z3Visualizer/Z3Visualizer/bin/Debug/Z3AxiomProfiler.exe" root)
                  boogie-friends-profile-analyzer-executable
                  "/home/clement/documents/internships/MSR/code/Z3Visualizer/Z3Visualizer/bin/Debug/Z3AxiomProfiler.exe")))

(with-eval-after-load 'dafny-mode
  (add-to-list 'dafny-prover-background-args "/printTooltips")
  (add-to-list 'dafny-prover-background-args "/autoTriggers:1"))

(defun setup-dafny ()
  (setq-local flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(add-hook 'dafny-mode-hook #'setup-dafny)
