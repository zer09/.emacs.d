;;; html-export.el --- Convert buffer contents to HTML

;;; Commentary:
;;

;;; Code:

(require 'dash)

;;; Face attributes

(defun htmle--face-value-nil-p (val)
  (memq val '(nil unspecified)))

(defun htmle--face-attributes-1 (face)
  "Convert FACE to an alist of properties.
This is a helper function for `htmle--face-attributes'.  It
doesn't resolve inheritance."
  (cond
   ((facep face)
    (face-all-attributes face (selected-frame)))
   ((listp face) ;; FIXME
    (json--plist-to-alist face))))

(defun htmle--merge-face-attribute (attr value faces)
  "Compute value of ATTR by merging VALUE with anonymous FACES."
  (-reduce-from
   (lambda (value fallback-alist)
     (let ((fallback (alist-get attr fallback-alist)))
       (merge-face-attribute attr value fallback)))
   value (cdr faces)))

(defun htmle--expand-faces-remove-inheritance-1 (face)
  "Convert FACE to a list of anonymous faces in inheritance order."
  (let ((parent (alist-get :inherit face)))
    (if (htmle--face-value-nil-p parent)
        (list face)
      (cons face (htmle--expand-faces-remove-inheritance-1
                  (face-all-attributes parent (selected-frame)))))))

(defun htmle--expand-faces-remove-inheritance (faces)
  "Expand list of FACES to remove inheritance."
  (cl-loop for face in faces
           append (htmle--expand-faces-remove-inheritance-1 face)))

(defun htmle--merge-face-alists (faces)
  "Merge anonymous FACES.
Ignores inheritance."
  (cl-loop for (attr . value) in (car faces)
           collect (cons attr (htmle--merge-face-attribute attr value faces))))

(defun htmle--face-attributes (face)
  "Transform FACE to an anonymous face.
FACE may be a list of face, such as in the `face' property."
  (htmle--merge-face-alists
   (htmle--expand-faces-remove-inheritance
    (if (listp face)
        (mapcar #'htmle--face-attributes-1 face)
      (list (htmle--face-attributes-1 face))))))

;;; CSS

(defun htmle--css-pair-to-string (pair)
  "Render PAIR as a CSS assignment."
  (when pair
    (format "%s:%s;" (car pair) (cdr pair))))

(defvar htmle--css-rules nil
  "Alist of CSS rules to export for current conversion.")

(defun htmle--add-css-rule (name generator &rest args)
  (unless (assoc name htmle--css-rules)
    (push `(,name . ,(apply generator args)) htmle--css-rules)))

(defconst htmle--bold-levels
  '((thin . 100)
    (ultralight . 100)
    (ultra-light . 100)
    (extralight . 200)
    (extra-light . 200)
    (light . 300)
    (book . 400)
    (demilight . 400)
    (semilight . 400)
    (semi-light . 400)
    (normal . 500)
    (medium . 500)
    (regular . 500)
    (demi . 600)
    (demibold . 600)
    (semibold . 600)
    (semi-bold . 600)
    (bold . 700)
    (extrabold . 800)
    (extra-bold . 800)
    (black . 900)
    (ultrabold . 900)
    (ultra-bold . 900)))

(defun htmle--css-pairs-from-face-1 (prop val)
  (pcase prop
    (`:family
     (cons "font-family" val))
    (`:height
     (setq val (face-attribute-merged-with :height val '(default) (selected-frame)))
     (cons "font-size" (format "%.2fpt" (/ val 10.0))))
    (`:slant
     (cons "font-style" val))
    (`:weight
     (cons "font-weight" (format "%d" (cdr (assoc val htmle--bold-levels)))))))

(defun htmle--css-pairs-from-face-2 (value css-prop fallback fallback-prop inverse-video)
  (unless (and (htmle--face-value-nilp value) (not inverse-video))
    (when inverse-video
      (setq value (face-attribute-merged-with fallback-prop fallback '(default))))
    `((,css-prop . ,value))))

(defun htmle--css-pairs-from-face (face)
  (setq face (htmle--face-attributes face))
  (let ((inverse-video (eq (cdr (assoc :inverse-video face)) t))
        (fg (cdr (assoc :foreground face)))
        (bg (cdr (assoc :background face))))
    (append (cl-loop for (prop . val) in face
                     unless (htmle--face-value-nil-p val)
                     collect (htmle--css-pairs-from-face-1 prop val))
            (htmle--css-pairs-from-face-2 fg "color" bg :background inverse-video)
            (htmle--css-pairs-from-face-2 bg "background-color" fg :foreground inverse-video))))

(defun htmle--style-from-css-pairs (css-pairs)
  (-when-let* ((rendered (-keep #'htmle--css-pair-to-string css-pairs)))
    `(style . ,(string-join rendered))))

(defun htmle--style-from-face (face)
  (htmle--style-from-css-pairs (htmle--css-pairs-from-face face)))

(defun htmle--face-renderer (props-alist html-tree)
  `(span (,(htmle--style-from-face
            (alist-get 'face props-alist)))
         ,html-tree))

(defun htmle--css-pairs-from-composition (char)
  `(("content" . ,(format "\"%c\"" char))))

(defun htmle--composition-rule-name (char)
  (format "comp-%s"
          (replace-regexp-in-string
           " " "-" (get-char-code-property char 'name))))

(defun htmle--register-composition (char)
  (let ((name (htmle--composition-rule-name char)))
    (htmle--add-css-rule (concat "." name ":after")
                         #'htmle--css-pairs-from-composition char)
    name))

(defun htmle--composition-renderer-1 (char html-tree)
  `(span nil
         (span (,(htmle--style-from-css-pairs '(("color" . "transparent")
                                                ("display" . "inline-block")
                                                ("width" . "0")
                                                ("z-index" . "-1"))))
               ,html-tree)
         (span ((class . ,(htmle--register-composition char)))
               nil)))

(defun htmle--composition-renderer (props-alist html-tree)
  (pcase (alist-get 'composition props-alist)
    (`nil html-tree)
    ((or `((,_ . ,char)) `(,_ ,_ [,char]))
     (htmle--composition-renderer-1 char html-tree))
    (other (error "Unsupported composition %S on %S" other html-tree))))

(defun htmle--display-renderer (props-alist html-tree)
  (pcase (alist-get 'display props-alist)
    (`nil html-tree)
    (`(raise ,amount)
     `(span (,(htmle--style-from-css-pairs `(("position" . "relative")
                                             ("bottom" . ,(format "%fem" amount)))))
            ,html-tree))
    (other (error "Unsupported display property %S on %S" other html-tree))))

(defvar htmle--renderers '(htmle--display-renderer htmle--composition-renderer htmle--face-renderer)
  "Functions used to render a span of text.
Each function takes an alist of text properties and a tree, and
returns a new tree.")

(defun htmle--render (props string)
  (-reduce-from
   (lambda (tree renderer)
     (funcall renderer props tree))
   string htmle--renderers))

(defun htmle-region-to-html (beg end)
  (let* ((props (json--plist-to-alist (text-properties-at beg))))
    (htmle--render props (buffer-substring-no-properties beg end))))

;;; Conversion to strings

(defun htmle--tree-to-string-attributes (attributes)
  (string-join
   (-keep (lambda (pair)
            (when (consp pair)
              (format " %s=\"%s\"" (car pair) (cdr pair))))
          attributes)))

(defun htmle--tree-to-string (tree)
  (pcase tree
    ((pred stringp)
     (xml-escape-string tree))
    (`(litteral . ,string)
     string)
    (`(,tag ,attributes)
     (format "<%s%s/>"
             tag (htmle--tree-to-string-attributes attributes)))
    (`(,tag ,attributes . ,body)
     (format "<%s%s>%s</%s>"
             tag (htmle--tree-to-string-attributes attributes)
             (mapconcat #'htmle--tree-to-string body "")
             tag))))

(defun htmle--forest-to-string (&rest trees)
  (mapconcat #'htmle--tree-to-string trees "\n"))

(defun htmle--css-rules-to-string ()
  (cl-loop for (rule . css-pairs) in htmle--css-rules
           concat (format "%s { %s } "
                          rule (mapconcat #'htmle--css-pair-to-string css-pairs ""))))

;;; Main function

(defun htmle-do ()
  "Convert current buffer to HTML."
  (font-lock-ensure)
  (let* ((source-buffer (current-buffer))
         (other-buffer (get-buffer-create (concat (buffer-name source-buffer) ".html")))
         (spans (cl-loop for cur-point = (point-min) then next-point
                         for next-point = (next-property-change cur-point)
                         collect (cons cur-point (or next-point (point-max)))
                         while next-point)))
    (with-current-buffer other-buffer
      (erase-buffer)
      (delete-all-overlays)
      (setq htmle--css-rules `(("*" . (("font-family" . "'Ubuntu Mono', Consolas, monospace, 'XITS Math', Symbola !important")))))
      (let ((body (cl-loop for (beg . end) in spans
                           collect (with-current-buffer source-buffer
                                     (htmle-region-to-html beg end)))))
        (insert
         (htmle--forest-to-string
          `(litteral . "<!DOCTYPE html>")
          `(html nil
                 (head nil
                       (meta ((charset . "utf-8")))
                       (title nil ,(buffer-name source-buffer))
                       (style ((type . "text/css"))
                              (litteral . ,(htmle--css-rules-to-string))))
                 (body (,(htmle--style-from-face 'default))
                       (pre (,(htmle--style-from-face 'default)) ,@body))))))
      (write-file (expand-file-name (buffer-name) temporary-file-directory))
      (pop-to-buffer (current-buffer)))))

(provide 'html-export)
;;; html-export.el ends here
