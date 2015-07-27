(defun setup-gpg-maybe ()
  (when (and buffer-file-name (string-match epa-file-name-regexp buffer-file-name))
    (message "Backup inhibited for this file")
    (setq-local backup-inhibited t)
    (auto-save-mode -1)))

(add-hook 'find-file-hook #'setup-gpg-maybe)
