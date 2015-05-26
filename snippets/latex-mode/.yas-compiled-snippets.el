;;; Compiled snippets and support files for `latex-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'latex-mode
                     '(("dm" "\n\\[ $0 \\]\n" "displaymath" nil nil nil nil nil nil)
                       ("ed" "\\documentclass{scrartcl}\n\n\\usepackage{boilerplate}\n${morepackages}\n\n\\ttl{${title}}\n\n\\begin{document}\n\\maketitle\n\n$0\n\\end{document}\n" "emptydoc" nil nil nil nil "" nil)
                       ("i" "\\\\(`yas-selected-text`$1\\\\)$0" "inlinemath" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Sun Apr 19 20:45:23 2015
