(require 'dash)
(require 'browse-url)
(require 'htmlfontify)
(require 'font-lock)

(defun hfy+-next-composition-change (pos)
  (next-single-property-change pos 'composition))

(defun hfy+-next-composition (pos)
  (if (hfy+-get-composition pos) pos
    (hfy+-next-composition-change pos)))

(defun hfy+-composition-end (pos)
  (when (hfy+-get-composition pos)
    (or (hfy+-next-composition-change pos) (point-max))))

(defun hfy+-get-composition (pos)
  (-when-let* ((comp-info (get-text-property pos 'composition)))
    (pcase comp-info
      ((seq `(,_ . ,char)) char)
      ((seq  _ _ `[,char]) char)
      (_                   (error "Invalid composition {%s}" comp-info)))))

(defun hfy+-substitute-composition (beg)
  (-when-let* ((end   (hfy+-composition-end beg))
               (comp  (hfy+-get-composition beg))
               (props (text-properties-at beg)))
    (goto-char beg)
    (delete-region beg end)
    (insert (apply #'propertize (char-to-string comp) props))
    (point)))

(defun hfy+-custom-page-header (file style)
  (hfy-default-header file (concat "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">"
                                   "<style type=\"text/css\">*{font-family: Consolas,Cambria !important;}</style>"
                                   style)))

(defun hfy+-buffer ()
  "Same as `htmlfontify-buffer', but also renders composed characters."
  (interactive)
  (let* ((source            (current-buffer))
         (hfy-optimizations (cons 'skip-refontification hfy-optimizations))
         (hfy-page-header   #'hfy+-custom-page-header)
         (temp-file         (make-temp-file "hfy" nil ".html")))
    (when (bound-and-true-p font-lock-mode)
      (funcall (if (fboundp 'font-lock-ensure) #'font-lock-ensure #'font-lock-fontify-buffer)))
    (with-current-buffer (get-buffer-create " *HTML*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-buffer-substring source)
        (goto-char (point-min))
        (cl-loop for start = (hfy+-next-composition (point))
                 while (and start (hfy+-substitute-composition start)))
        (-when-let* ((html-buffer (htmlfontify-buffer)))
          (with-current-buffer html-buffer
            (write-region (point-min) (point-max) temp-file nil 'no-message))
          (kill-buffer html-buffer)
          (browse-url-of-file temp-file))))))
