;;; cam.el --- Make Emacs screencasts -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Clément Pit-Claudel

;; Author: Clément Pit--Claudel <clement@clem-w50-mint>
;; Keywords: convenience, multimedia
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5"))
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Start recording with `cam-start-recording'.  Stop recording with
;; `cam-stop-recording' (bound to C-M-S-s while recording).

;;; Code:

(require 'cl-lib)
(require 'let-alist)

(defvar cam--frames nil
  "Frames of current recording.
A list of frames.  Each element in this list should be an alist.
Known keys currently include:

- `fnames': A list of file names; the first element of the list
  is the main file name; subsequent elements are auxiliary files
  to delete when cleaning up temporaries.

- `timestamp': A timestamp in `current-time' format, indicating
  when the frame was captured.")

(defvar cam--last-command-keys-vector nil
  "Key vector of last command.")

(defvar cam-speedup-factor 1.0
  "Speedup factor for generated gifs.")

(defvar cam-final-delay 100
  "Final delay (in one-hundredth of a second) before looping.")

(defvar cam-emacs-only t
  "Whether to include other frames in the screencast.
In addition to other windows, these other frames include menus
and pop-ups.")

(defvar cam-pre-record-hook ()
  "Hook run before starting a screencast.")

(defvar cam-post-record-hook ()
  "Hook run after completing a screencast.")

(defun cam--delete-frames ()
  "Delete all files in `cam-frames'."
  (dolist (frame cam--frames)
    (dolist (fname (alist-get 'fnames frame))
      (ignore-errors
        (when (file-exists-p fname)
          (delete-file fname)))))
  (setq cam--frames nil))

(defun cam--directory ()
  "Create and return cam.el's temporary directory."
  (let ((dir (expand-file-name "cam.el" temporary-file-directory)))
    (make-directory dir t)
    dir))

(defun cam--cleanup ()
  "Cleanup internal variables."
  (setq cam--frames nil
        cam--last-command-keys-vector nil))

(defun cam--redisplay ()
  "Force a redisplay."
  (force-mode-line-update t)
  (force-window-update)
  (redisplay t))

(defun cam-start-recording-1 (clear-msg)
  "Redisplay, then start collecting frames.
With non-nil CLEAR-MSG, call (message nil) first."
  (cam--cleanup)
  (setq-default cam-mode t)
  (when clear-msg (message nil))
  (cam--redisplay)
  (add-hook 'post-redisplay-hook #'cam--post-redisplay-hook)
  (add-hook 'post-command-hook #'cam--update-keys)
  (add-hook 'pre-command-hook #'cam--update-keys))

(defun cam-start-recording ()
  "Start recording, offering to save previously running capture."
  (interactive)
  (cam-stop-recording)
  (run-hook-with-args 'cam-pre-record-hook)
  (run-with-timer 0 nil #'cam-start-recording-1 t))

(defalias 'cam-rec #'cam-start-recording)

(defun cam-stop-recording-1 ()
  "Stop collecting frames."
  (setq-default cam-mode nil)
  (remove-hook 'post-redisplay-hook #'cam--post-redisplay-hook)
  (remove-hook 'post-command-hook #'cam--update-keys)
  (remove-hook 'pre-command-hook #'cam--update-keys))

(defun cam--compute-delay (current-t next-t)
  "Compute delay from CURRENT-T to NEXT-T."
  (if (null next-t) cam-final-delay
    (let ((delta (time-subtract next-t current-t)))
      (max 1 (round (/ (* 100 (float-time delta)) cam-speedup-factor))))))
;; (pcase-let* ((`(,hi ,lo ,μ ,_) current-t)
;;              (`(,hi-n ,lo-n ,μ-n ,_) next-t))
;;   (max 1 (round (/ (+ (* (- hi-n hi) (expt 10 8))
;;                       (* (- lo-n lo) (expt 10 2))
;;                       (/ (- μ-n μ) (expt 10 4)))
;;                    cam-speedup-factor))))))

(defun cam-annot (frame)
  "Compute annotation for FRAME."
  (let-alist frame
    ;; FIXME escape specials
    (key-description .keys)))

(defun cam-frames2gif-args-1 (frame frames)
  "Compute partial `convert' args for FRAME and FRAMES."
  (let-alist frame
    (let* ((next-frame-timestamp (alist-get 'timestamp (car frames)))
           (delay (cam--compute-delay .timestamp next-frame-timestamp))
           (annot (cam-annot frame)) ;; ↓ FIXME nil
           (annot-form (when nil `("-annotate" "+11+37" ,annot))))
      `("-delay" ,(int-to-string delay)
        "(" ,@annot-form ,(car .fnames) ")"))))

(defun cam-frames2gif-args (frames fname)
  "Compute `convert' args to render FRAMES as FNAME."
  (let ((cmds nil)
        (fidx 0))
    (while frames
      (cl-incf fidx)
      (push (cam-frames2gif-args-1 (pop frames) frames) cmds))
    (let ((fringe-color (face-attribute 'fringe :background))
          (fringe-width (frame-parameter (selected-frame) 'left-fringe)))
      `("-gravity" "SouthEast" ;; FIXME make font configurable
        "-font" "/usr/share/fonts/truetype/ubuntu-font-family/UbuntuMono-R.ttf"
        "-pointsize" "48"
        "-fill" "white"
        "-undercolor" "#00000080"
        ,@(apply #'append (reverse cmds))
        "-strip"
        "-loop" "0"
        "-bordercolor" ,fringe-color
        "-border" ,(format "0x%d" fringe-width)
        "-layers" "optimize"
        ;; "-monitor"
        ,fname))))

(defun cam--call (prog args)
  "Call PROG with ARGS; error out if prog fails."
  (with-temp-buffer
    (let* ((process-connection-type nil)
           (process-adaptive-read-buffering nil)
           (proc (apply #'start-process "*cam.el conversion*" (current-buffer) prog args))
           (message-log-max nil)
           (refresh-rate-fps 20)
           (nticks 0))
      (while (process-live-p proc)
        (let ((ndots (/ (* 3 (cl-incf nticks)) refresh-rate-fps)))
          (message "Assembling screencast%s" (make-string ndots ?.)))
        (sit-for (/ 1.0 refresh-rate-fps)))
      (message nil)
      (let ((retv (process-exit-status proc)))
        (unless (eq retv 0)
          (let ((cmd (mapconcat #'shell-quote-argument (cons prog args) " ")))
            (error "Call to “%s” failed.
Status:  ‘%s’
Command: ‘%s’
Output:  ‘%s’" prog retv cmd (buffer-string))))))
    (message "%s" (mapconcat #'shell-quote-argument (cons prog args) " "))))

(defun cam-save-1 (gif-fname)
  "Compose `cam--frames' into a gif named GIF-FNAME."
  (unless (string-match-p "\\.gif\\'" gif-fname)
    (user-error "`%s' does not end in .gif" gif-fname))
  (cam--call "convert" (cam-frames2gif-args (reverse cam--frames) gif-fname)))

(defun cam--prompt-and-save ()
  "Save frames under user-selected file name."
  (when cam--frames
    (unwind-protect
        (when (y-or-n-p "Save recording? ")
          (let* ((msg (format "Save %d frames as: " (length cam--frames)))
                 (fname (read-file-name msg)))
            (cam-save-1 fname)
            (browse-url fname)))
      (cam--delete-frames))))

(defun cam-save ()
  "Stop recording and prompt to save a gif."
  (interactive)
  (cam-stop-recording-1)
  (cam--prompt-and-save)
  (run-hook-with-args 'cam-post-record-hook))

;; (defun ~/show-last-command ()
;;   (message (format "%S %S"
;;                    (this-command-keys-vector)
;;                    (key-description (this-command-keys-vector)))))

;; (add-hook 'pre-command-hook #'~/show-last-command)
;; (add-hook 'post-command-hook #'~/show-last-command)
;; (remove-hook 'pre-command-hook #'~/show-last-command)
;; (remove-hook 'post-command-hook #'~/show-last-command)

(defalias 'cam-stop-recording #'cam-save)

(defun cam-window-id ()
  "Return the ID of the current X window."
  (frame-parameter nil 'outer-window-id))

(defun cam-dump-using-import (fname &rest aux-names)
  "Use import to save a screenshot under FNAME (no extension added).
AUX-NAMES are registered in return value."
  (let ((args `("-window" ,(cam-window-id) "-quality" "0" ,fname)))
    (unless cam-emacs-only (push "-screen" args))
    (apply #'call-process "import" nil nil nil args))
  (cons fname aux-names))

(defun cam-dump-png-using-import (fname)
  "Use import to save a screenshot under FNAME.png."
  (cam-dump-using-import (concat fname ".png")))

(defun cam-dump-mpc-using-import (fname)
  "Use import to save a screenshot under FNAME.png."
  (cam-dump-using-import (concat fname ".mpc") (concat fname ".cache")))

(defun cam-dump-using-xwd (fname)
  "Use xwd to save a screenshot under FNAME.xwd."
  (setq fname (concat fname ".xwd"))
  (let ((args `("-silent" "-id" ,(cam-window-id) "-out" ,fname)))
    (unless cam-emacs-only (push "-screen" args))
    (apply #'call-process "xwd" nil nil nil args))
  (list fname))

(defvar cam-dump-function #'cam-dump-using-xwd
  "Function to call to capture a screenshot.
Called with a single argument (a file name without an extension).
Should return a list of file names, whose CAR is the main one.")

(defun cam-screenshot ()
  "Save screenshot of current frame."
  (let* ((timestamp (current-time))
         (time-str (mapconcat #'number-to-string timestamp "-"))
         (fname-sans-ext (expand-file-name time-str (cam--directory)))
         (fnames (funcall cam-dump-function fname-sans-ext)))
    (push `((timestamp . ,timestamp)
            (fnames . ,fnames)
            (keys . ,cam--last-command-keys-vector))
          cam--frames)))

(defun cam--post-redisplay-hook ()
  "Function to put in `post-redisplay-hook'."
  (let ((inhibit-redisplay t))
    (cam-screenshot)))

(defun cam--update-keys ()
  "Record last command."
  (let ((keys-vector (this-command-keys-vector)))
    (unless (eq keys-vector '[]) ;; Surprising data from `post-command-hook'
      (setq cam--last-command-keys-vector keys-vector)))) ;; (cam--post-redisplay-hook)

(defun cam-setup-ui ()
  "Hide auxiliary UI elements."
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1))

(defun cam-setup-cursor ()
  "Disable blinking cursor and set to a bar."
  (blink-cursor-mode -1)
  (setq-default cursor-type 'bar))

(defun cam-default-pre-record-hook ()
  "Enable a decent set of defaults for making screencasts."
  (with-no-warnings
    (setq-default ido-max-window-height 1
                  truncate-lines t))
  (ido-mode 1)
  (cam-setup-ui)
  (cam-setup-cursor))

;; FIXME
(add-hook 'cam-pre-record-hook #'cam-default-pre-record-hook)

(defvar cam-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-S-s") #'cam-stop-recording)
    map))

(define-minor-mode cam-mode
  "Record an Emacs screencast.

To stop recording, press \\<cam-mode-map>\\[cam-stop-recording]."
  :lighter " 📹"
  :global t
  (if cam-mode
      (cam-start-recording)
    (cam-stop-recording)))

;; (process-lines "optipng" "-o3" fname)

(provide 'cam)
;;; cam.el ends here
