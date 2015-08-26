;;; demo-mode.el --- Set up emacs for projector demos upon opening -*- lexical-binding: t -*-

;; Copyright (C) 2015 Clément Pit--Claudel
;; Author: Clément Pit--Claudel <clement.pitclaudel@live.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'dash)
(require 'files)

(defvar demo-mode--large-font-size 200)
(defvar demo-mode--original-font-size nil)

;; (defvar-local demo-mode--overlay nil)

(defun demo-mode--recenter (buf)
  (-when-let (wnd (get-buffer-window buf))
    (with-selected-window wnd
      (goto-char (point-min))
      (forward-line)
      (recenter 0))))

(define-minor-mode demo-mode
  "Sets a number of display variable to make things look good on a projector."
  :lighter nil
  (load-theme 'tangomod t)
  (if demo-mode
      (progn
        ;; Save and enlarge font
        (unless demo-mode--original-font-size
          (setq demo-mode--original-font-size
                (face-attribute 'default :height)))
        (set-face-attribute 'default nil :height demo-mode--large-font-size)
        ;; Add an empty line at the end of the file
        (goto-char (point-max))
        (unless (eq (point) (point-at-bol))
          (insert "\n"))
        ;; Restrict to region
        (goto-char (point-min))
        (when (set-auto-mode-1)
          (forward-line)
          (narrow-to-region (point-at-bol) (point-max))))
    (widen)
    ;; Reset the font
    (when demo-mode--original-font-size
      (set-face-attribute 'default nil :height demo-mode--original-font-size))))

(provide 'demo-mode)
;;; demo-mode.el ends here
