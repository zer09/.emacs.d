(defun org-beamer-headless ()
  "Export current file to LaTeX, ommitting the preamble."
  (interactive)
  (require 'org)
  (let* ((fname (or buffer-file-name ""))
         (org-fname (replace-regexp-in-string "\.org\\'" ".tex" fname))
         ;; (user-full-name "Clément Pit-\\kern0pt-Claudel") ;; Doesn't seem to work
         (org-export-show-temporary-export-buffer nil))
    (if (string= fname org-fname)
        (error "Not sure where to save the LaTeX file")
      (org-beamer-export-as-latex nil nil nil t nil)
      (with-current-buffer "*Org BEAMER Export*"
        ;; (indent-region (point-min) (point-max)) ;; Breaks verbatim
        (set-buffer-file-coding-system 'utf-8)
        (write-file org-fname)
        (kill-buffer)))))

(defvar ~/org/todo-suspend-cookie-updates nil)

(defun ~/org/todo-update-summary (n-done n-not-done)
  "Track progress of children tasks (N-DONE, N-NOT-DONE)."
  (let ((old-state (org-get-todo-state))
        (new-state (cond
                    ((= n-not-done 0) "DONE")
                    ((= n-done 0) "TODO")
                    (t "STARTED"))))
    (unless (equal old-state new-state)
      (org-todo new-state))))

(defun ~/org/todo-add-cookie (&optional empty)
  "Add a possibly EMPTY statistics cookie on current line."
  (when (save-excursion (org-goto-first-child))
    (save-match-data
      (unless (save-excursion
                (beginning-of-line)
                (re-search-forward "\\[[0-9]*\\(%\\|/[0-9]*\\)\\]" (point-at-eol) t))
        (save-excursion
          (end-of-line)
          (just-one-space)
          (insert "[/]")
          (save-excursion
            (org-goto-first-child)
            (unless empty
              (org-update-parent-todo-statistics))))))))

(defun ~/org/todo-state-change ()
  "Add statistics cookie to node and parent nodes."
  (unless ~/org/todo-suspend-cookie-updates
    (let ((~/org/todo-suspend-cookie-updates t))
      (save-excursion
        (~/org/todo-add-cookie)
        (while (org-up-heading-safe)
          (~/org/todo-add-cookie))))))

(defun ~/truncate-time (time)
  "Remove the time component of TIME."
  (pcase-let ((`(_ _ _ ,day ,month ,year) (decode-time time)))
    (encode-time 0 0 0 day month year)))

(defun ~/org/check-before-or-on-date (&optional date)
  "Check if there are deadlines or scheduled entries before or on DATE."
  (interactive "P")
  (cond
   ((null date) (setq date (~/truncate-time (org-current-time))))
   ((consp date) (setq date (org-read-date nil t))))
  (message "DATE: %S" date)
  (let ((case-fold-search nil)
        (regexp (org-re-timestamp 'scheduled-or-deadline))
        (callback
         (lambda ()
           "Check whether this task is scheduled in the past, and not completed."
           (let ((t1 (org-time-string-to-time (match-string 1))))
             (and (not (save-match-data (org-entry-is-done-p)))
                  (or (equal t1 date)
                      (time-less-p t1 date)))))))
    (message "%d entries before %s"
             (org-occur regexp nil callback) date)))

(with-eval-after-load 'org
  (setq-default org-log-done 'time
                org-support-shift-select t
                org-use-fast-todo-selection t
                org-odd-levels-only t
                org-hide-leading-stars t
                org-completion-use-ido t
                org-latex-listings t
                org-special-ctrl-a/e nil
                org-return-follows-link nil ;; Can't add a newline after timestamp otherwise
                org-ellipsis " …" ;; ▸ 🞂 ▼ ▶ ⏩
                org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "PENDING(w)" "LATER(l)" "|" "DONE(d)" "CANCELLED(c)"))
                ;; org-todo-keywords '((sequence "🌕(t)" "🌖(s)" "⏳(w)" "📅(l)" "|" "✓(d)" "✗(c)")) ;; 🌗
                ;; org-todo-keywords '((sequence "👉(t)" "✅(s)" "⏳(w)" "📅(l)" "|" "✓(d)" "✗(c)"))
                ;; org-todo-keywords '((sequence "👉(t)" "✅(s)" "⏳(w)" "📅(l)" "|" "✔(d)" "✘(c)"))
                ;; org-todo-keywords '((sequence "☐(t)" "✅(s)" "⏳(w)" "📅(l)" "|" "☑(d)" "☒(c)"))
                )

  (set-face-attribute 'org-todo nil :bold nil)
  (set-face-attribute 'org-done nil :bold nil)

  (define-key org-mode-map (kbd "C-c t") #'~/org/check-before-or-on-date)

  ;; Move usual S-* commands to keypad
  (define-key org-mode-map (kbd "<S-kp-right>") #'org-shiftright)
  (define-key org-mode-map (kbd "<S-kp-left>") #'org-shiftleft)
  (define-key org-mode-map (kbd "<S-kp-up>") #'org-shiftup)
  (define-key org-mode-map (kbd "<S-kp-down>") #'org-shiftdown)

  (define-key org-mode-map (kbd "<S-right>") nil)
  (define-key org-mode-map (kbd "<S-left>") nil)
  (define-key org-mode-map (kbd "<S-up>") nil)
  (define-key org-mode-map (kbd "<S-down>") nil)

  (define-key org-mode-map (kbd "<M-kp-6>") #'org-shiftmetaright)
  (define-key org-mode-map (kbd "<M-kp-4>") #'org-shiftmetaleft)
  (define-key org-mode-map (kbd "<M-kp-8>") #'org-shiftmetaup)
  (define-key org-mode-map (kbd "<M-kp-2>") #'org-shiftmetadown)

  (define-key org-mode-map (kbd "<C-kp-6>") #'org-shiftcontrolright)
  (define-key org-mode-map (kbd "<C-kp-4>") #'org-shiftcontrolleft)
  (define-key org-mode-map (kbd "<C-kp-8>") #'org-shiftcontrolup)
  (define-key org-mode-map (kbd "<C-kp-2>") #'org-shiftcontroldown)

  (define-key org-mode-map (kbd "<C-S-right>") nil)
  (define-key org-mode-map (kbd "<C-S-left>") nil)
  (define-key org-mode-map (kbd "<C-S-up>") nil)
  (define-key org-mode-map (kbd "<C-S-down>") nil)

  ;; Move S-[rlud] to M-S-[rlup]
  (define-key org-mode-map (kbd "<M-S-right>") #'org-shiftright)
  (define-key org-mode-map (kbd "<M-S-left>") #'org-shiftleft)
  (define-key org-mode-map (kbd "<M-S-up>") #'org-shiftup)
  (define-key org-mode-map (kbd "<M-S-down>") #'org-shiftdown)

  (define-key org-mode-map (kbd "<C-M-up>") #'org-shiftmetaup)
  (define-key org-mode-map (kbd "<C-M-down>") #'org-shiftmetadown)
  (define-key org-mode-map (kbd "<C-M-left>") #'org-shiftmetaleft)
  (define-key org-mode-map (kbd "<C-M-right>") #'org-shiftmetaright))

(add-hook 'org-after-todo-statistics-hook #'~/org/todo-update-summary)
(add-hook 'org-after-todo-state-change-hook #'~/org/todo-state-change)

(defun ~/org/add-line-spacing ()
  (add-to-list (make-local-variable 'font-lock-extra-managed-props) 'display)
  ;; (font-lock-add-keywords nil '(("^\\*\\*\\*\\( \\)" 1 '(face nil display (space . (:relative-height 1.0))) append)))
  (font-lock-add-keywords nil '(("^\\*\\*\\( \\)" 1 '(face nil display (space . (:relative-height 1.2))) append)))
  (font-lock-add-keywords nil '(("^\\*\\( \\)" 1 '(face nil display (space . (:relative-height 1.5))) append))))

(require 'dash)

(defconst ~/org/prettification-alist
  (-zip-pair '("TODO" "STARTED" "PENDING" "LATER" "DONE" "CANCELLED")
             '("🌕" "🌖" "⏳" "📅" "✓" "✗")))

(defun ~/org/make-todo-regexp (header)
  "Construct a TODO regexp from HEADER."
  (format "^\\*+\\(?: \\)\\(%s\\)" (regexp-quote header)))

(defun ~/org/undo-hardcoded-prettification ()
  "Perform replacements suggested by inverting `~/org/prettification-alist'.
That is, change hard-coded prettifications to regular keywords."
  (interactive)
  (dolist (new-header (apply #'append org-todo-sets))
    (-when-let* ((old-header (cdr (assoc new-header ~/org/prettification-alist)))
                 (old-header-re (~/org/make-todo-regexp old-header)))
      (goto-char (point-min))
      (while (re-search-forward old-header-re nil t)
        (replace-match new-header t t nil 1)))))

;; (defun ~/org/compute-composition (from to)
;;   "Compute a composition property to show FROM as TO."
;;   (compose-string from 0 (length from)
;;                   (cdr
;;                    (apply #'append
;;                           (mapcar (lambda (c)
;;                                     `((Br . Bl) ,c))
;;                                   (string-to-list to))))))

(defun ~/org/prettify-one (header)
  "Prettify HEADER according to `~/org/prettification-alist'."
  (-when-let* ((re (~/org/make-todo-regexp header))
               (rep (cdr (assoc header ~/org/prettification-alist)))
               ;; (composition
               ;; (compose-spec `(face nil composition ,composition))
               (display-spec `(face nil display ,rep)))
    `(,re 1 ',display-spec)))

(defun ~/org/prettify-keywords ()
  "Prettify Org TODO heads according to `~/org/prettification-alist'."
  ;; (setq-local adaptive-wrap-extra-indent 0) ;; Indent TODO entries properly
  (font-lock-add-keywords nil (-keep #'~/org/prettify-one (apply #'append org-todo-sets))))

(defun ~/org/update-cookies ()
  (interactive)
  (when (derived-mode-p 'org-mode)
    (org-update-statistics-cookies 'all)))

(defun ~/org/setup ()
  "."
  (flyspell-mode)
  (~/org/add-line-spacing)
  (~/org/prettify-keywords)
  (add-hook 'before-save-hook #'~/org/update-cookies nil t))

(add-hook 'org-mode-hook #'~/org/setup)
