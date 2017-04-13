;;; company-coq-bedrock.el --- Company-coq extension to prettify Bedrock code -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Clément Pit-Claudel

;; Author: Clément Pit-Claudel <clement.pitclaudel@live.com>
;; Keywords: convenience, languages

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

;; Setup:

;; 1. Copy this file to .emacs.d
;; 2. Add the following to your .emacs:
;;      (load-file "~/.emacs.d/company-coq-bedrock.el")

;;; Code:

(require 'company-coq)

(defface company-coq-bedrock-word-face
  '((t :underline "white"))
  "Face used to highlight bedrock words."
  :group 'company-coq-faces)

(defun company-coq-bedrock--word-display-spec (w _pad-to)
  "Prettify a Bedrock word W.
_PAD-TO is ignored."
  (let* ((bin (replace-regexp-in-string "~" "" w))
         (num (string-to-number bin 2))
         (str (format "0%s%s (0%s%x)"
                      ;; (format (format "%%-%ds" pad-to) …)
                      (propertize "b" 'face 'font-lock-constant-face) bin
                      (propertize "x" 'face 'font-lock-constant-face) num)))
    (font-lock-append-text-property 0 (length str) 'face 'company-coq-bedrock-word-face str)
    str))

(defun company-coq-bedrock--word-fl-spec ()
  "Compute a font-lock specification for current match."
  (let ((spec (company-coq-bedrock--word-display-spec
               (match-string-no-properties 1)
               (- (match-end 0) (match-beginning 0)))))
    `(face nil display ,spec)))

(defconst company-coq-bedrock--font-lock-keywords
  '(("WO\\(\\(?:~[01]\\)+\\)" (0 (company-coq-bedrock--word-fl-spec)))))

(with-eval-after-load 'company-coq
  (company-coq-define-feature bedrock (arg)
    "Prettify bedrock words."
    (company-coq-do-in-all-buffers
      (add-to-list 'font-lock-extra-managed-props 'display)
      (pcase arg
        (`on (font-lock-add-keywords nil company-coq-bedrock--font-lock-keywords))
        (`off (font-lock-remove-keywords nil company-coq-bedrock--font-lock-keywords)))
      (company-coq-request-refontification))))

(provide 'company-coq-bedrock)
;;; company-coq-bedrock.el ends here
