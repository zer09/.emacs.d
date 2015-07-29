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
                  flycheck-inferior-dafny-executable "/build/MSR/dafny/Binaries/DafnyRepl.exe"
                  boogie-friends-profile-analyzer-executable "/build/MSR/vcc/vcc/Tools/Z3Visualizer/Z3Visualizer/bin/Debug/Z3AxiomProfiler.exe"))
  (when-os '(windows-nt cygwin)
    (setq-default flycheck-z3-smt2-executable "C:/MSR/dafny/Binaries/z3.exe"
                  flycheck-dafny-executable "C:/MSR/dafny/Binaries/Dafny.exe"
                  flycheck-boogie-executable "C:/MSR/boogie/Binaries/Boogie.exe"
                  flycheck-inferior-dafny-executable "C:/MSR/dafny/Binaries/DafnyRepl.exe"
                  boogie-friends-profile-analyzer-executable "C:/Program Files (x86)/Microsoft Research/Vcc/Binaries/Z3AxiomProfiler.exe")))

(defun setup-boogie-friends ()
  (diminish-undo 'flycheck-mode)
  (add-to-list 'dafny-prover-background-args "/printTooltips")
  (setq-local flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(add-hook 'boogie-friends-hook #'setup-boogie-friends)
