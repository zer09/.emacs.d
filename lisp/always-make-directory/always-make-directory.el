;;; always-make-directory --- Create directory without prompting after running find-file

;;; Commentary:

;; Ensures the target directory exists after each call to find-file. This saves
;; you from constantly having to run "M-x make-directory RET RET".

;;; Code:

(defun amkdir-file-not-found-function ()
  "Create the parent directory of the current buffer's file."
  (let* ((fname (buffer-file-name))
         (fdir  (and fname (file-name-directory fname))))
    (when fdir
      (unless (file-exists-p fdir)
        (with-demoted-errors "Could not auto-create directory: %S"
          (make-directory fdir t))))))

(pop find-file-not-found-functions)
(add-to-list 'find-file-not-found-functions #'amkdir-file-not-found-function)

(provide 'always-make-directory)
;;; always-make-directory.el ends here
