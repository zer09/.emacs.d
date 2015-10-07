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

;; Adapted from http://www.emacswiki.org/emacs/move-text.el
(defun move-text-internal (arg)
  (if (and mark-active transient-mark-mode)
      (progn (when (> (point) (mark))
               (exchange-point-and-mark))
             (let ((column (current-column))
                   (text (delete-and-extract-region (point) (mark))))
               (forward-line arg)
               (move-to-column column t)
               (set-mark (point))
               (insert text)
               (exchange-point-and-mark)
               (setq deactivate-mark nil)))
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg)
          (when (< arg 0)
            (forward-line -1)))
        (forward-line -1))
      (move-to-column column t))))

(defun move-line-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-line-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(defun make-title-line ()
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
  "Open a line above the current one, move there, and indent."
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
    (mapc #'my-set-font-fallbacks (fontset-list)) ; Changing the default size creates a new fontset, with incorrect fallbacks
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
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (fringe-mode '(4 . 4))
  (setq-default mode-line-format (unless hide-modeline (default-value 'mode-line-format))
                cursor-type nil))

(defun sh (arg)
  (interactive "P")
  (let ((same-window-regexps '("shell")))
    (call-interactively #'shell)))

;; Debugging ;;

(defmacro with-timer (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

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
