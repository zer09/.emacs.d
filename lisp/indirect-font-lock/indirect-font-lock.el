;;; indirect-font-lock.el --- Highlight parts of comments and strings as code  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Clément Pit-Claudel

;; Author: Clément Pit-Claudel <clement.pitclaudel@live.com>
;; Keywords: faces

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

;;

;;; Code:

(defvar-local indirect-font-lock--temp-buffers nil
  "Alist of (MODE-FN . BUFFER).
These are temporary buffers, used for highlighting.")

(defun indirect-font-lock--kill-temp-buffers ()
  "Kill buffers in `indirect-font-lock--temp-buffers'."
  (mapc #'kill-buffer (mapcar #'cdr indirect-font-lock--temp-buffers))
  (setq indirect-font-lock--temp-buffers nil))

(defun indirect-font-lock--make-buffer-for-mode (mode-fn)
  "Create a temporary buffer for MODE-FN.
The buffer is created and initialized with MODE-FN only once;
further calls with the same MODE-FN reuse the same buffer."
  (let ((buffer (cdr (assoc mode-fn indirect-font-lock--temp-buffers))))
    (unless buffer
      (setq buffer (generate-new-buffer (format " *%S-highlight*" mode-fn)))
      (push (cons mode-fn buffer) indirect-font-lock--temp-buffers)
      (with-current-buffer buffer
        (funcall mode-fn)
        (setq-local kill-buffer-query-functions nil)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer))
    buffer))

(defun indirect-font-lock--copy-faces-to (buffer offset)
  "Copy faces from current buffer to BUFFER, starting at OFFSET."
  (let ((start (point-min))
        (making-progress t)
        (offset (- offset (point-min))))
    (while making-progress
      (let ((end (next-single-property-change start 'face nil (point-max))))
        (if (< start end)
            (font-lock-prepend-text-property (+ start offset) (+ end offset)
                                     'face (get-text-property start 'face)
                                     buffer)
          (setq making-progress nil))
        (setq start end)))))

(defun indirect-font-lock--fontify-as (mode-fn from to)
  "Use buffer in MODE-FN to fontify FROM..TO.

In other word, fontify FROM..TO would as if it had been alone in its own
buffer, in major mode MODE-FN."
  (let ((str (buffer-substring-no-properties from to))
        (original-buffer (current-buffer)))
    (with-current-buffer (indirect-font-lock--make-buffer-for-mode mode-fn)
      (insert str)
      (font-lock-fontify-region (point-min) (point-max))
      (indirect-font-lock--copy-faces-to original-buffer from))))

(defun indirect-font-lock-highlighter (group mode-fn)
  "Font-lock highlighter using an indirect buffer.
Fontify GROUP as if it had been alone in its own buffer, in major
mode MODE-FN."
  (save-match-data
    (indirect-font-lock--fontify-as mode-fn (match-beginning group) (match-end group)))
  '(face nil))

(provide 'indirect-font-lock)
;;; indirect-font-lock.el ends here
