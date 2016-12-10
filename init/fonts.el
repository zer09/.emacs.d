;;;;;;;;;;;;;;;;;;;
;; Font settings ;;
;;;;;;;;;;;;;;;;;;;

(defvar ~/main-font "Ubuntu Mono")

(defun my-configure-one-fontset (fontset &optional size)
  "Add fallbacks to FONTSET with SIZE."
  (let* ((size (when size `(:size ,(/ size 10.0))))
         (base-spec (apply #'font-spec :name ~/main-font size))
         (emoji-spec (apply #'font-spec :name (format "Segoe UI Emoji monospacified for %s" ~/main-font) size))
         (symbol-spec (apply #'font-spec :name (format "XITS Math monospacified for %s" ~/main-font) size))
         (fallback-spec (apply #'font-spec :name (format "Symbola monospacified for %s" ~/main-font) size))
         (cjk-spec (apply #'font-spec :name "WenQuanYi Micro Hei Mono" size)))
    (set-fontset-font fontset 'unicode base-spec nil)
    (set-fontset-font fontset 'unicode emoji-spec nil 'append)
    (set-fontset-font fontset 'unicode symbol-spec nil 'append)
    (set-fontset-font fontset 'unicode fallback-spec nil 'append)
    (set-fontset-font fontset (cons ?😱 ?😱) "Symbola" nil 'prepend)
    (set-fontset-font fontset (cons #x2009 #x2009) "Symbola" nil 'prepend) ;; Thin space
    (dolist (cjk-block '((#x3000 . #x303F)
                         (#x3040 . #x309F)
                         (#x30A0 . #x30FF)
                         (#x3400 . #x4DFF)
                         (#x4E00 . #x9FFF)
                         (#xF900 . #xFAFF)
                         (#x20000 . #x2A6DF)
                         (#x2A700 . #x2B73F)
                         (#x2B740 . #x2B81F)
                         (#x2B820 . #x2CEAF)
                         (#x2F800 . #x2FA1F)))
      (set-fontset-font fontset cjk-block cjk-spec nil 'append))))

(defun my-configure-all-fontsets (&optional _frame)
  "Set fallbacks for all fontsets.
Former SIZE argument removed because it broke
`text-scale-adjust'."
  (interactive)
  (when (fboundp 'fontset-list)
    (mapc #'my-configure-one-fontset (fontset-list))))

(my-configure-all-fontsets)
(add-to-list 'after-make-frame-functions #'my-configure-all-fontsets)

(defun ~/fonts/add-inheritance (face new-parent)
  "Make FACE inherit from NEW-PARENT."
  (let ((inherits (face-attribute face :inherit)))
    (setq inherits
          (if (listp inherits)
              (copy-sequence inherits)
            (list inherits)))
    (set-face-attribute
     face nil :inherit (cons new-parent (delq new-parent inherits)))))

(with-eval-after-load 'ruler-mode
  (set-face-attribute 'ruler-mode-default nil :box nil)
  (~/fonts/add-inheritance 'ruler-mode-default 'fixed-pitch))

(setq-default use-default-font-for-symbols nil)

(set-face-attribute 'fixed-pitch nil :family ~/main-font)
(set-face-attribute 'variable-pitch nil :family "Corbel")
(set-face-attribute 'vertical-border nil :foreground "darkgrey")

;; (with-eval-after-load 'which-func
;;   (set-face-attribute 'which-func nil :foreground 'unspecified)
;;   (set-face-attribute 'which-func nil :inherit font-lock-doc-face))

;; Font selection ;;
;; Faster than custom-set-faces
;; But still slower than using .Xresources
;; ;; (set-face-attribute 'default nil
;; ;;                     :inherit nil
;; ;;                     :background "#ffffff"
;; ;;                     :foreground "#000000"
;; ;;                     :height 98
;; ;;                     :family "Ubuntu Mono")
;; Replaced by .Xresources containing "Emacs.font: Ubuntu Mono-10"
