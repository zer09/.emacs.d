(defun renumber (begin end)
  (interactive "r")
  (save-match-data
    (save-excursion
      (goto-char begin)
      (let ((num 0))
        (while (re-search-forward "[0-9]+" end t)
          (replace-match (format "%d" (setq num (1+ num)))))))))
