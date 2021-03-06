(defun org-beamer-headless ()
  "Export current file to LaTeX, ommitting the preamble."
  (interactive)
  (require 'org)
  (let* ((fname (or buffer-file-name ""))
         (org-fname (replace-regexp-in-string "\.org\\'" ".tex" fname))
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
  "Filter to include only deadlines or scheduled entries before or on DATE."
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

(defun ~/org/reftex ()
  (interactive)
  (pcase (read-key "Key? [<ret>pt]")
    ((or ?t ?\C-m) (reftex-citet))
    (?p (reftex-citep))))

(with-eval-after-load 'org
  (require 'org-eldoc nil t)
  (require 'reftex)
  (setq-default org-log-done 'time
                org-support-shift-select t
                org-use-fast-todo-selection t
                org-hide-leading-stars t
                org-completion-use-ido t
                org-special-ctrl-a/e nil
                org-return-follows-link nil ;; Can't add a newline after timestamp otherwise
                org-ellipsis " …" ;; ▸ 🞂 ▼ ▶ ⏩
                org-pretty-entities t
                ;; Settings below affect export
                org-odd-levels-only t
                org-latex-listings nil
                org-latex-prefer-user-labels t
                org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "PENDING(w)" "LATER(l)"
                                              "|" "DONE(d)" "CANCELLED(c)"))
                ;; org-todo-keywords '((sequence "🌕(t)" "🌖(s)" "⏳(w)" "📅(l)" "|" "✓(d)" "✗(c)")) ;; 🌗
                ;; org-todo-keywords '((sequence "👉(t)" "✅(s)" "⏳(w)" "📅(l)" "|" "✓(d)" "✗(c)"))
                ;; org-todo-keywords '((sequence "👉(t)" "✅(s)" "⏳(w)" "📅(l)" "|" "✔(d)" "✘(c)"))
                ;; org-todo-keywords '((sequence "☐(t)" "✅(s)" "⏳(w)" "📅(l)" "|" "☑(d)" "☒(c)"))
                )

  (set-face-attribute 'org-todo nil :bold nil)
  (set-face-attribute 'org-done nil :bold nil)

  (define-key org-mode-map (kbd "C-c [") #'~/org/reftex)
  (define-key org-mode-map (kbd "C-c C-t") #'~/org/check-before-or-on-date)

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

  (define-key org-mode-map (kbd "<C-M-up>") #'org-metaup)
  (define-key org-mode-map (kbd "<C-M-down>") #'org-metadown)
  (define-key org-mode-map (kbd "<C-M-left>") #'org-shiftmetaleft)
  (define-key org-mode-map (kbd "<C-M-right>") #'org-shiftmetaright)

  (define-key org-mode-map (kbd "C-j") #'org-return)
  (define-key org-mode-map (kbd "RET") #'org-return-indent))

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
  (format "^\\*+\\(?: \\)\\(%s\\) " (regexp-quote header)))

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

(defun ~/org/ensure-monospace-indentation ()
  (dolist (face '(org-meta-line
                  org-code
                  org-table
                  org-verbatim
                  org-document-info-keyword
                  org-block
                  org-block-begin-line
                  org-block-end-line))
    (~/fonts/add-inheritance face 'fixed-pitch))
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  (dolist (re (list "^[[:space:]]+\\(\\([0-9]+\\.\\|[*+-]\\)? +\\)?"
                    (format "^\\*+ +\\(%s +\\)?" org-todo-regexp)))
    (font-lock-add-keywords nil `((,re 0 '(face fixed-pitch) append)) 'append)))

;; Fixed in Org 9
;;
;; (defun ~/org/make-region-monospace (_lang start end)
;;   "Add a fixed-pitch face to START..END."
;;   (font-lock-append-text-property start end 'face 'fixed-pitch))
;;
;; (with-eval-after-load 'org-src
;;   (advice-add #'org-src-font-lock-fontify-block :after #'~/org/make-region-monospace))

(defun ~/org/update-cookies ()
  (interactive)
  (when (derived-mode-p 'org-mode)
    ;; The ‘let’ binding prevents Org from getting confused about non-existent files
    (let ((buffer-file-name nil))
      (org-update-statistics-cookies 'all))))

(defun ~/org/add-creation-date ()
  "Add creation date to current entry, if needed."
  (require 'org-expiry)
  (save-excursion
    (org-back-to-heading)
    (org-expiry-insert-created)))

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("mitthesis"
                 "\\documentclass{mitthesis}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(with-eval-after-load 'org
  (setq org-latex-default-packages-alist
        '(("AUTO" "polyglossia" t)
          ("" "fontspec" t)
          ("" "fixltx2e" nil)
          ("" "graphicx" t)
          ("" "grffile" t)
          ("" "longtable" nil)
          ("" "wrapfig" nil)
          ("" "rotating" nil)
          ("normalem" "ulem" t)
          ("" "amsmath" t)
          ("" "textcomp" t)
          ("" "amssymb" t)
          ("" "capt-of" nil)
          ("" "hyperref" nil))))

(dolist (opt '(org-latex-title-command
               org-latex-toc-command
               org-html-mathjax-template))
  (put opt 'safe-local-variable 'stringp))

(dolist (opt '(org-log-done
               org-odd-levels-only
               org-latex-prefer-user-labels
               org-latex-listings
               org-html-htmlize-output-type
               org-html-preamble
               org-html-postamble
               org-html-head-include-scripts
               org-html-head-include-default-style))
  (put opt 'safe-local-variable 'booleanp))

(dolist (opt '(org-export-headline-levels))
  (put opt 'safe-local-variable 'integerp))

(with-eval-after-load 'ox-latex
  (when (require 'esh-org nil t)
    (esh-org-activate)))

(defun ~/org/kill-fontification-buffer ()
  "Kill org's temporary fontification buffers."
  (interactive)
  (dolist (buf (buffer-list))
    (when (string-match-p (regexp-quote " *org-src-fontification:") (buffer-name buf))
      (kill-buffer buf))))

(defun ~/org/setup ()
  "."
  (flyspell-mode)
  (~/org/add-line-spacing)
  (~/org/prettify-keywords)
  (~/org/ensure-monospace-indentation)
  (when (and buffer-file-name (file-exists-p buffer-file-name))
    (let ((dir (file-name-directory (buffer-file-name))))
      (dolist (f (directory-files dir t "\\.bib\\'"))
        (add-to-list 'reftex-default-bibliography f))))
  ;; Disable: property drawers are ugly
  ;; (add-hook 'org-after-todo-state-change-hook #'~/org/add-creation-date nil t)
  (add-hook 'before-save-hook #'~/org/update-cookies nil t))

(add-hook 'org-mode-hook #'~/org/setup)
