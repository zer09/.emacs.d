(defun ~/after-change (start end len)
  (let* ((src (current-buffer))
         (dst (get-buffer-create (format "*%s-mirror*" (buffer-name src)))))
    (with-current-buffer dst
      (cond
       ((not (= (buffer-size src) (buffer-size dst)))
        (erase-buffer)
        (insert-buffer src))
       (t
        (delete-region start (+ start len))
        (goto-char start)
        (insert-buffer-substring src start end)))
      (dolist (win (get-buffer-window-list))
        (set-window-point win end)))
    (message "%S %S %S %S" (current-buffer) start end len)))

(add-hook 'after-change-functions #'~/after-change nil t)
