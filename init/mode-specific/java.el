(defun ~/java ()
  (setq-local parens-require-spaces nil)
  (setq-local prettify-symbols-alist'(("<=" . ?≤)
                                      (">=" . ?≥)
                                      ("!=" . ?≠)
                                      ("||" . ?∨)
                                      ("&&" . ?∧)
                                      ("null" . ?⊥)))
  (prettify-symbols-mode))

(add-hook 'java-mode-hook #'~/java)
