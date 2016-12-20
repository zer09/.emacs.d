;;; company-defcustom.el --- Autocomplete setq forms using defcustom types  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Clément Pit-Claudel

;; Author: Clément Pit-Claudel <clement@clem-w50-mint>
;; Keywords: abbrev, convenience

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

(require 'company)
(require 'cus-edit)

(defun ~/completions-for-type (type)
  "Get completions for defcustom TYPE."
  (pcase type
    (`(choice . ,choices)
     (seq-mapcat #'~/completions-for-type choices))
    (`(const :tag ,tag ,value)
     (list (propertize (prin1-to-string value) 'annotation tag)))))

(defvar-local ~/assignment-target nil
  "Symbol of current variable being set.
This can't be tucked as an annotation to prefix, because the
prefix itself can be an empty string, and that can't be
propertized.")

(defun ~/prefix ()
  "Read prefix at point."
  (when (looking-back "(setq \\(\\(?:\\s_\\|\\sw\\)+\\) \\(.*\\)" (point-at-bol))
    (let ((assignment-target (intern (match-string-no-properties 1)))
          (prefix (match-string-no-properties 2)))
      (when (custom-variable-p assignment-target)
        (setq ~/assignment-target assignment-target)
        (cons prefix t)))))

;; FIXME could use a small circular buffer to cache completions
(defun ~/candidates (prefix)
  "Find completions of PREFIX based on `~/assignment-target'."
  (let* ((type (custom-variable-type ~/assignment-target))
         (completions (~/completions-for-type type))
         (prefix-re (concat "^" (regexp-quote prefix))))
    (seq-filter (lambda (s) (string-match-p prefix-re s)) completions)))

(defun ~/backend (command &optional arg &rest _)
  "A company-mode backend for HTML tags.
COMMAND, ARG: see `company-backends'."
  (interactive (list 'interactive))
  (pcase command
    (`interactive (company-begin-backend '~/company-defcustom))
    (`prefix (~/prefix))
    (`candidates (~/candidates arg))
    (`annotation (get-text-property 0 'annotation arg))
    (`sorted t)
    (`duplicates nil)
    (`ignore-case nil)))

;; (add-to-list 'company-backends #'~/backend)

(provide 'company-defcustom)
;;; company-defcustom.el ends here
