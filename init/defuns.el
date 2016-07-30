;;;;;;;;;;;;;;;;;;;;;
;;; Custom defuns ;;;
;;;;;;;;;;;;;;;;;;;;;

;;; Editing

(defun beginning-of-whitespace (skip)
  (goto-char (point-at-bol))
  (when skip (skip-chars-forward skip)))

(defun end-of-whitespace (skip)
  (goto-char (point-at-eol))
  (when skip (skip-chars-backward skip)))

(defun line-wrapped-p ()
  (or (/= (save-excursion (end-of-visual-line) (point)) (point-at-eol))
      (/= (save-excursion (beginning-of-visual-line) (point)) (point-at-bol))))

(defun adjust-padding (skip count)
  (beginning-of-whitespace skip)
  (just-one-space count)
  (end-of-whitespace skip)
  (just-one-space count))

(defun empty-line-p (&optional skip)
  (save-excursion
    (beginning-of-line)
    (when skip (skip-chars-forward (concat " " skip)))
    (eq (point) (point-at-eol))))

(defun center-visual-line (skip)
  (interactive '("/(*)+!"))
  (when (not (empty-line-p skip))
    (adjust-padding skip 0)
    (when (not (line-wrapped-p))
      (cl-loop while (not (line-wrapped-p))
               for w = 1 then (1+ w)
               do (adjust-padding skip w)
               finally do (adjust-padding skip (1- w))))))

(defun center-visual-lines (skip &optional start end)
  (interactive (cons "/(*)+!"
                     (when (region-active-p) (list (region-beginning) (region-end)))))
  (setq start (or start (point)))
  (setq end (or end (point)))
  (save-excursion
    (goto-char start)
    (cl-loop do (progn (beginning-of-line)
                       (center-visual-line skip)
                       (end-of-line))
             while (and (= 0 (forward-line 1)) (<= (point) end)))))

(defun move-line (n)
  "Move the current line up by N lines."
  (interactive "p")
  (if (derived-mode-p 'org-mode)
      (if (> 0 n)
          (org-shiftmetaup n)
        (org-shiftmetadown (- n)))
    (let* ((col (current-column))
           (start (point-at-bol))
           (end (min (point-max) (1+ (point-at-eol))))
           (line-text (delete-and-extract-region start end)))
      (forward-line n)
      (insert line-text)
      (forward-line -1)
      (forward-char col))))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (- (or n 1))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (or n 1)))

(defun make-title-line ()
  "Wrap current line in comment delimiters."
  (interactive)
  (forward-line 0)
  (re-search-forward "^[; ]*\\(.*?\\)[; ]*$" (point-at-eol))
  (forward-line 0)
  (let* ((title  (match-string-no-properties 1))
         (tlen   (length title))
         (spacer (make-string (+ tlen 8) ?\;)))
    (kill-line)
    (insert spacer)
    (newline)
    (insert (concat ";;; " title " ;;;"))
    (newline)
    (insert spacer)
    (newline 2)))

(defun open-and-indent-next-line ()
  "Split current line and indent both."
  (interactive)
  (save-excursion
    (insert "\n")
    (indent-according-to-mode))
  (indent-according-to-mode))

;; http://stackoverflow.com/questions/9688748/emacs-comment-uncomment-current-line
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (if (functionp #'comment-line)
      (call-interactively #'comment-line)
    (let (beg end) ; Fallback for Emacs < 25
      (if (region-active-p)
          (setq beg (region-beginning) end (region-end))
        (setq beg (line-beginning-position) end (line-end-position)))
      (comment-or-uncomment-region beg end)
      (forward-line))))

(defun sort-words (fold-case beg end)
  "Sort words in region alphabetically (with \\[universal-argument], sorts in reverse)."
  (interactive "*P\nr")
  (let ((sort-fold-case (or fold-case (bound-and-true-p sort-fold-case))))
    (sort-regexp-fields nil "\\(\"\\(?2:[^\"]+\\)\"\\)\\|\\(?2:\\w+\\)" "\\2" beg end)))

;;; Navigation

(defun find-file-here ()
  (interactive)
  (let ((ido-use-filename-at-point 'guess)
        (default-directory (if (equal buffer-file-name nil)
                               default-directory
                             (file-name-directory (buffer-file-name)))))
    (call-interactively #'ido-find-file)))

(defun next-slide (sep)
  (interactive (list "(******************************************************************************)"))
  (when (search-forward sep nil t)
    (recenter 0)))

(defun prev-slide (sep)
  (interactive (list "(******************************************************************************)"))
  (unless (search-backward sep nil t)
    (goto-char (point-min)))
  (recenter 0))

;;; Large fonts

(defvar ~/default-font-size nil)
(defvar ~/large-font-size 180)

(defun use-large-font ()
  "Change to a large font."
  (interactive)
  (unless ~/default-font-size
    (setq ~/default-font-size (face-attribute 'default :height)))
  (dolist (frame (frame-list))
    (set-face-attribute 'default frame :height ~/large-font-size)))

(defun use-regular-font ()
  "Change to a large font."
  (interactive)
  (when (numberp ~/default-font-size)
    (dolist (frame (frame-list))
      (set-face-attribute 'default frame :height ~/default-font-size))
    (setq ~/default-font-size nil)))

(defalias 'use-small-font 'use-regular-font)

;;; Snippets

(defun sheebang ()
  (interactive)
  (insert "#!/usr/bin/env "))

(defun today (&optional arg)
  (interactive "P")
  (if (consp arg) () (insert "# "))
  (insert (format-time-string "%Y-%m-%d (%A)"))
  (newline 2))

(defun now (&optional arg)
  (interactive "P")
  (if (consp arg) () (insert "## "))
  (insert (format-time-string "%Y-%m-%d (%A) %H:%M"))
  (newline 2))

(defun ~/quote-region-1 (left right &optional beg end count)
  (unless beg
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (point) end nil)))
  (unless count
    (setq count 1))
  (save-excursion
    (goto-char (or end beg))
    (dotimes (_ count) (insert right)))
  (save-excursion
    (goto-char beg)
    (dotimes (_ count) (insert left)))
  (if (and end (characterp left)) ;; Second test handles the ::`` case
      (goto-char (+ (* 2 count) end))
    (goto-char (+ count beg))))

(defun quote-region (&optional beg end repeat)
  "Insert quotes aroung BEG..END.
With prefix REPEAT, repeat process twice."
  (interactive (list nil nil current-prefix-arg))
  (let ((quotes (pcase (read-char "Quote type?")
                  (?\` (if (or (derived-mode-p #'lisp-mode)
                               (derived-mode-p #'emacs-lisp-mode))
                           `(?\` . ?\')
                         `(?\` . ?\`)))
                  (?\' `(?\‘ . ?\’))
                  (?\" `(?\“ . ?\”))
                  (?\[ `(?\[ . ?\]))
                  (?\{ `(?\{ . ?\}))
                  (?\< `(?\< . ?\>))
                  (?\: `("::`" . ?\`))
                  (c   `(,c . ,c))))
        (count (if repeat 2 1)))
    (~/quote-region-1 (car quotes) (cdr quotes) beg end count)))

(defun ~/coqtop (beg end)
  (interactive (list (region-beginning) (region-end)))
  (replace-regexp "^Coq < " "      " nil beg end)
  (indent-rigidly beg end -3)
  (goto-char beg)
  (insert ".. coqtop:: all\n\n"))

(defun ~/rst-coq-action ()
  (interactive)
  (pcase (read-char "Command?")
    (?g (~/quote-region-1 ":g:`" "`"))
    (?n (~/quote-region-1 ":n:`" "`"))
    (?t (~/quote-region-1 ":token:`" "`"))
    (?m (~/quote-region-1 ":math:`" "`"))
    (?: (~/quote-region-1 "::`" "`"))
    (?` (~/quote-region-1 "``" "``"))
    (?c (~/coqtop (region-beginning) (region-end)))))

(defun hide-trailing-whitespace ()
  (interactive)
  (setq-local show-trailing-whitespace nil))

;;; Interaction
(require 'cl-lib)

(defun shred ()
  "Delete current file and kill buffer."
  (interactive)
  (let ((fname (buffer-file-name))
        (autosave-fname buffer-auto-save-file-name))
    (set-buffer-modified-p nil)
    (kill-buffer (current-buffer))
    (when fname
      (ignore-errors (delete-file fname))
      (ignore-errors (delete-file autosave-fname)))))

(defun kill-frame-or-emacs (arg)
  "Kill current frame.
If there are no other frames, or with prefix ARG, kill Emacs."
  (interactive "P")
  (if (and (cdr (frame-list)) (not (consp arg)))
      (progn
        (save-some-buffers arg t)
        (delete-frame))
    (save-buffers-kill-emacs)))

(defun undedicate-all ()
  (interactive)
  (cl-loop for window in (window-list)
           do (set-window-dedicated-p window nil)))

(defvar original-font-size nil)

(defun set-font-size-in-all-fontsets (size)
  (dolist (frame (frame-list))
    ;; Modifying fontsets directly works as well, and it doesn't create new ones
    ;; (set-face-attribute 'default frame :height new-size)
    (my-configure-all-fontsets frame size)))

(defun adjust-font-size (delta)
  (let* ((old-size (face-attribute 'default :height))
         (new-size (max (max delta (- delta)) (min 400 (+ delta old-size)))))
    (setq original-font-size (or original-font-size old-size))
    (set-font-size-in-all-fontsets new-size)
    (message "Font size set to %d (was %d)" (face-attribute 'default :height) old-size)))

(defun zoom-in ()
  (interactive)
  (adjust-font-size +10))

(defun zoom-out ()
  (interactive)
  (adjust-font-size -10))

(defun zoom-reset ()
  (interactive)
  (when original-font-size
    (set-font-size-in-all-fontsets original-font-size)))

(defun prepare-for-screenshot (&optional hide-modeline)
  (interactive "P")
  (load-theme 'tangomod t)
  (ruler-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (setq-default mode-line-format (unless hide-modeline (default-value 'mode-line-format))
                cursor-type nil))

(defun sh (arg)
  (interactive "P")
  (let ((same-window-regexps '("shell")))
    (call-interactively #'shell)))

(defun dired-kill-buffers ()
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

(defun ~/company-manual-begin ()
  "Run `company-manual-begin' if appropriate; otherwise, fallback to previous binding."
  (interactive)
  (if (and (bound-and-true-p company-mode)
           (not buffer-read-only))
      (company-manual-begin)
    (let* ((keybindings-minor-mode nil)
           (original-func (key-binding (this-command-keys-vector) t)))
      (when original-func
        (call-interactively original-func)))))

(defun ~/compile ()
  "Same as `compile', with bells and whistles.
Add -C if command is make and a Makefile can be found, and always
run in comint mode."
  (interactive)
  (let* ((command (eval compile-command))
         (makefile-dir (locate-dominating-file default-directory "Makefile"))
         (is-default-dir (or (null makefile-dir)
                             (string= (expand-file-name default-directory)
                                      (expand-file-name makefile-dir)))))
    (when (string-match "make \\(?:-C \\([^'\" ]+\\|\"[^\"]+\"\\|'[^']+'\\)\\)?" command)
      (setq command (replace-match (if (not is-default-dir)
                                       (format "make -C %S " makefile-dir)
                                     "make ")
                                   nil t command)))
    (let ((current-prefix-arg (cons 4 nil))
          (compile-command command))
      (call-interactively #'compile))))

;; (defun htop ()
;;   "Run htop in `ansi-term'."
;;   (interactive)
;;   (let* ((bufname "*htop*")
;;          (buf (or (get-buffer bufname)
;;                   (with-current-buffer (ansi-term "/bin/bash" "htop")
;;                     (goto-char (point-max))
;;                     (insert "htop")
;;                     (newline nil t)))))
;;     (pop-to-buffer buf)))

;;; Small macros

(defmacro ~/check (&rest pairs)
  "Compute the sum of the evaled cadrs in PAIRS.
Example: (~/check (a 1) (b (+ 0.5 2)) (c 3)) ⇒ 6.5."
  (cl-assert (cl-every (apply-partially #'eq 2)
                       (mapcar #'length pairs)))
  `(+ ,@(mapcar #'cadr pairs)))

;;; Debugging

(defmacro with-profiler (&rest body)
  "Wrap each for in BODY in a timer macro."
  (declare (indent defun))
  `(progn
     ,@(mapcar (lambda (form)
                 `(let ((start (current-time)))
                    ,form
                    (message "[%.02f] %s"
                             (float-time (time-since start))
                             (quote ,form))))
               body)))
