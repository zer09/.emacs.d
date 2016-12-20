;;; bibtex-repository.el --- Add file links to BibTeX entries

;;; Commentary:
;; Use `bibtex-repository-add-file' to add a file name to the current BibTeX
;; entry.  The default name (M-n) from the key of the current entry, and
;; `bibtex-repository-path'.  If the selected name doesn't match the default
;; name, the command will offer to rename the file.

(require 'bibtex)
(require 'subr-x)
(require 'dash)

;;; Code:

(defgroup bibtex-repository nil
  "Easily add files to BibTeX entries."
  :group 'bibtex)

(defcustom bibtex-repository-path nil
  "Path to repository of articles.
If nil, use the directory of the current buffer's file."
  :group 'bibtex-repository
  :type 'directory)

(defun bibtex-repository--key ()
  "Read key of current entry."
  (ignore-errors
    (save-excursion
      (bibtex-beginning-of-entry)
      (when (looking-at bibtex-entry-maybe-empty-head)
        (-when-let* ((key (bibtex-key-in-head)))
          (unless (string-empty-p key)
            key))))))

(defun bibtex-repository--guess-path ()
  "Guess path of repository."
  (or bibtex-repository-path
      (-when-let* ((fname (buffer-file-name)))
        (file-name-directory fname))))

(defun bibtex-repository--guess-file-name (extension)
  "Use key to guess name of file with EXTENSION matching current entry."
  (-when-let* ((key (bibtex-repository--key)))
    (expand-file-name (concat key "." extension) (bibtex-repository--guess-path))))

(defun bibtex-repository--rename-file (from to interactive)
  "Rename FROM to TO.
If INTERACTIVE, prompt to confirm renaming.  If not INTERACTIVE,
and TO exists, signal an error."
  (let ((file-exists (file-exists-p to)))
    (when (and file-exists (not interactive))
      (user-error "%S already exists!" to))
    (when (and (not file-exists)
               (or (not interactive) (y-or-n-p (format "Rename %S to %S? " from to))))
      (rename-file from to)
      (message "Renamed %S to %S." from to)
      t)))

(defun bibtex-repository--pick-file ()
  "Pick a file for the current entry."
  (let* ((default-fname (bibtex-repository--guess-file-name "pdf"))
         (default-dir (bibtex-repository--guess-path))
         (fname (read-file-name "File: " default-dir default-fname t))
         (full-name (expand-file-name fname default-dir)))
    (when (file-exists-p full-name)
      (bibtex-repository--rename-file full-name default-fname t))
    default-fname))

(defun bibtex-repository--format-file-field (fname)
  "Prepare FNAME for insertion as a File BibTeX entry."
  (format "{%s:%s:%s}"
          (file-name-nondirectory fname)
          fname
          (upcase (file-name-extension fname nil))))

(defun bibtex-repository--parse-file-field (contents)
  "Parse CONTENTS of file field as name:link:type."
  (pcase (split-string contents ":")
    (`(,name ,link ,type) link)
    (_ contents)))

;;;###autoload
(defun bibtex-repository-add-file ()
  "Add file field to current BibTeX entry."
  (interactive)
  (let ((fname (bibtex-repository--pick-file)))
    (bibtex-make-field `("File" nil ,(bibtex-repository--format-file-field fname)) 'move-t nil t)))

(defun bibtex-repository--field-name (bounds)
  "Get name of field in BOUNDS."
  (buffer-substring-no-properties
   (bibtex-start-of-name-in-field bounds)
   (bibtex-end-of-name-in-field bounds)))

(defun bibtex-repository--field-text (bounds)
  "Get value of field in BOUNDS."
  (buffer-substring-no-properties
   (bibtex-start-of-text-in-field bounds)
   (bibtex-end-of-text-in-field bounds)))

(defun bibtex-repository--find-field (name)
  "Find field by NAME in current entry."
  (setq name (upcase name))
  (catch 'found
    (save-excursion
      (bibtex-beginning-of-entry)
      (bibtex-beginning-first-field)
      (let ((bounds))
        (while (setq bounds (bibtex-parse-field))
          (if (string= name (upcase (bibtex-repository--field-name bounds)))
              (throw 'found bounds)
            (goto-char (bibtex-end-of-field bounds))))))))

(defun bibtex-repository--replace-field-text (bounds new-text)
  "Replace text of field delimited by BOUNDS with NEW-TEXT."
  (save-excursion
    (goto-char (bibtex-start-of-text-in-field bounds))
    (delete-region (point) (bibtex-end-of-text-in-field bounds))
    (insert new-text)))

;;;###autoload
(defun bibtex-repository-rename-file ()
  "Automatically rename file that current entry points to."
  (interactive)
  (-if-let* ((key (bibtex-repository--key)))
      (-if-let* ((bounds (bibtex-repository--find-field "file")))
          (let* ((file-value (bibtex-repository--field-text bounds))
                 (fname (bibtex-repository--parse-file-field file-value)))
            (unless (file-exists-p fname)
              (user-error "Source file %S does not exist" fname))
            (let ((new-fname (bibtex-repository--guess-file-name (file-name-extension fname))))
              (when (bibtex-repository--rename-file fname new-fname t)
                (bibtex-repository--replace-field-text bounds (bibtex-repository--format-file-field new-fname)))))
        (user-error "No file in this entry"))
    (user-error "No key in this entry")))

(provide 'bibtex-repository)
;;; bibtex-repository.el ends here
