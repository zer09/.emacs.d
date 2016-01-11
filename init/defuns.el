;;;;;;;;;;;;;;;;;;;
;; Custom defuns ;;
;;;;;;;;;;;;;;;;;;;

;; Editing ;;

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
  "Move the current line up or down by N lines."
  (interactive "p")
  (let* ((col (current-column))
         (start (point-at-bol))
         (end (min (point-max) (1+ (point-at-eol))))
         (line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    (forward-line -1)
    (forward-char col)))

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
         (spacer (make-string (+ tlen 6) ?\;)))
    (kill-line)
    (insert spacer)
    (newline)
    (insert (concat ";; " title " ;;"))
    (newline)
    (insert spacer)
    (newline 2)))

(defun open-and-indent-next-line ()
  "SPlit current line and indent both."
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

;; Navigation ;;

(defun find-file-here ()
  (interactive)
  (when (not (equal buffer-file-name nil))
    (cd (file-name-directory (buffer-file-name))))
  (find-file-at-point))

(defun next-slide (sep)
  (interactive (list "(******************************************************************************)"))
  (when (search-forward sep nil t)
    (recenter 0)))

(defun prev-slide (sep)
  (interactive (list "(******************************************************************************)"))
  (when (search-backward sep nil t)
    (recenter 0)))

;; Snippets ;;

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
  (insert (format-time-string "%Y-%m-%d (%A) %H:%m"))
  (newline 2))

;; Interaction ;;
(require 'cl-lib)

(defun shred ()
  "Delete current file and kill buffer."
  (interactive)
  (let ((fname (buffer-file-name)))
    (set-buffer-modified-p nil)
    (kill-buffer (current-buffer))
    (when buffer-file-name
      (delete-file fname))))

(defun undedicate-all ()
  (interactive)
  (cl-loop for window in (window-list)
           do (set-window-dedicated-p window nil)))

(defvar original-font-size nil)

(defun adjust-font-size (delta)
  (let* ((old-size (face-attribute 'default :height))
         (new-size (max (max delta (- delta)) (min 400 (+ delta old-size)))))
    (setq original-font-size (or original-font-size old-size))
    (set-face-attribute 'default nil :height new-size)
    (my-reset-all-font-fallbacks) ; Changing the default size creates a new fontset, with incorrect fallbacks
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
    (set-face-attribute 'default nil :height original-font-size)))

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

;; Debugging ;;

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
