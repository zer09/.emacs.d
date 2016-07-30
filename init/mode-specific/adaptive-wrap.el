;; -*- lexical-binding: t; -*-

(defun ~/adaptive-wrap/add-fixed-pitch (str)
  "Add a fixed-pitch face to STR."
  (setq str (copy-sequence str))
  (font-lock-append-text-property 0 (length str) 'face 'fixed-pitch str)
  str)

(with-eval-after-load 'adaptive-wrap
  ;; The string that adaptive-wrap inserts before each line is most often taking
  ;; literally from the buffer's contents, but not always. In Org mode, for
  ;; example, when the text is
  ;;   - Some bullet
  ;; the prefix is “ ” (without the bullet).  This piece of advice ensures that
  ;; regardless of how the prefix string was obtained, it always is displayed in
  ;; fixed-pitch font.  This ensures that, even if the Org mode buffer is in
  ;; variable-pitch mode, the continuation lines are still indented properly.
  (advice-add 'adaptive-wrap-fill-context-prefix
              :filter-return #'~/adaptive-wrap/add-fixed-pitch))
