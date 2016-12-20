;; TODO autocompletion C-x 8 RET

(require 'cl-lib)

(defun company-unicode-prefix ()
  (word-at-point))

(defun company-unicode-candidates (prefix)
  (interactive)
  (let* ((prefix-re (concat "\\_<" (regexp-quote prefix))))
    (save-match-data
      (cl-loop for     pair
               in      (ucs-names)
               when    (string-match prefix-re (car pair))
               collect (propertize (car pair) 'annot (cdr pair) 'from (match-beginning 0) 'to (match-end 0))))))

(defun company-unicode-annot (arg)
  (char-to-string (get-text-property 0 'annot arg)))

(defun company-unicode (command &optional arg &rest ignored)
  "A company-mode backend for unicode characters."
  (interactive (list 'interactive))
  (pcase command
    (`interactive (company-begin-backend 'company-unicode))
    (`prefix (company-unicode-prefix))
    (`candidates (company-unicode-candidates arg))
    (`sorted t)
    (`duplicates nil)
    (`ignore-case nil)
    (`match (get-text-property 0 'to arg))
    (`annotation (company-unicode-annot arg))
    (`post-completion (progn (delete-char (- (length arg))) (insert (company-unicode-annot arg))))
    (`require-match 'never)))
