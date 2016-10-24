(require 'company)
(require 'yasnippet)

(provide 'company-html)

(defconst company-html-tag-regexp "<\\([a-zA-Z]+\\)[^>]*\\(>?\\)")

(defun company-html-prefix ()
  (save-match-data
    (when (looking-back company-html-tag-regexp)
      (match-string-no-properties 0))))

(defun company-html-candidates (prefix)
  (save-match-data
    (when (string-match (concat "\\`" company-html-tag-regexp "\\'") prefix)
      (list (concat prefix
                    (if (> 0 (length (match-string 2 prefix))) "" ">")
                    "$1</" (match-string 1 prefix) ">$0")))))

(defun company-html-post-completion (candidate)
  (yas-expand-snippet candidate (- (point) (length candidate)) (point)))

(defun company-html-tags (command &optional arg &rest ignored)
  "A company-mode backend for HTML tags."
  (interactive (list 'interactive))
  (pcase command
    (`interactive (company-begin-backend 'company-html-tags))
    (`prefix (company-html-prefix))
    (`candidates (company-html-candidates arg))
    (`sorted t)
    (`duplicates nil)
    (`ignore-case nil)
    (`post-completion (company-html-post-completion arg))
    (`require-match 'never)))
