;;; autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "../lisp/boogie-friends/emacs/boogie-mode"
;;;;;;  "../lisp/boogie-friends/emacs/boogie-mode.el" (21931 3625
;;;;;;  143553 925000))
;;; Generated autoloads from ../lisp/boogie-friends/emacs/boogie-mode.el

(add-to-list 'auto-mode-alist '("\\.bpl\\'" . boogie-mode))

(autoload 'boogie-mode "../lisp/boogie-friends/emacs/boogie-mode" "\
Major mode for editing Boogie programs.

\\{boogie-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../lisp/boogie-friends/emacs/dafny-mode" "../lisp/boogie-friends/emacs/dafny-mode.el"
;;;;;;  (22060 8666 984906 530000))
;;; Generated autoloads from ../lisp/boogie-friends/emacs/dafny-mode.el

(autoload 'dafny-test-suite-open-diff "../lisp/boogie-friends/emacs/dafny-mode" "\


\(fn DFY-NAME)" t nil)

(autoload 'dafny-test-suite-accept-diff "../lisp/boogie-friends/emacs/dafny-mode" "\


\(fn DFY-NAME)" t nil)

(add-to-list 'auto-mode-alist '("\\.dfy\\'" . dafny-mode))

(autoload 'dafny-mode "../lisp/boogie-friends/emacs/dafny-mode" "\
Major mode for editing Dafny programs.

\\{dafny-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../lisp/boogie-friends/emacs/z3-smt2-mode"
;;;;;;  "../lisp/boogie-friends/emacs/z3-smt2-mode.el" (22066 25085
;;;;;;  986575 876000))
;;; Generated autoloads from ../lisp/boogie-friends/emacs/z3-smt2-mode.el

(add-to-list 'auto-mode-alist '("\\.smt2\\'" . z3-smt2-mode))

(autoload 'z3-smt2-mode "../lisp/boogie-friends/emacs/z3-smt2-mode" "\
Major mode for editing SMT2 programs.

\\{z3-smt2-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../lisp/htmlfontify/htmlfontify+" "../lisp/htmlfontify/htmlfontify+.el"
;;;;;;  (21915 32867 566401 988000))
;;; Generated autoloads from ../lisp/htmlfontify/htmlfontify+.el

(autoload 'hfy+-buffer "../lisp/htmlfontify/htmlfontify+" "\
Same as `htmlfontify-buffer', but also renders composed characters.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("../lisp/boogie-friends/emacs/boogie-friends-pkg.el"
;;;;;;  "../lisp/boogie-friends/emacs/boogie-friends.el" "../lisp/boogie-friends/emacs/dafny-docs.el"
;;;;;;  "../lisp/boogie-friends/emacs/inferior-dafny.el") (22096
;;;;;;  59092 887250 105000))

;;;***

(provide 'autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; autoloads.el ends here
