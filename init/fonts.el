;;;;;;;;;;;;;;;;;;;
;; Font settings ;;
;;;;;;;;;;;;;;;;;;;

(defvar my-main-font "Ubuntu Mono")

(defun my-configure-one-fontset (frame size fontset)
  "Add fallbacks to FONTSET on FRAME with SIZE."
  (let* ((size (when size `(:size ,(/ size 10.0))))
         (base-spec (apply #'font-spec :name my-main-font size))
         (emoji-spec (apply #'font-spec :name (format "Segoe UI Emoji monospacified for %s" my-main-font) size))
         (symbol-spec (apply #'font-spec :name (format "Symbola monospacified for %s" my-main-font) size))
         (cjk-spec (apply #'font-spec :name "WenQuanYi Micro Hei Mono" size)))
    (set-fontset-font fontset 'unicode base-spec frame)
    (set-fontset-font fontset 'unicode emoji-spec frame 'append)
    (set-fontset-font fontset 'unicode symbol-spec frame 'append)
    (set-fontset-font fontset (cons #x2009 #x2009) "Symbola" nil 'prepend) ;; Thin space
    (dolist (cjk-block '((#x2F800 . #x2FA1F) (#xF900 . #xFAFF) (#x20000 . #x2A6DF) (#x3400 . #x4DFF) (#x4E00 . #x9FFF)))
      (set-fontset-font fontset cjk-block cjk-spec frame 'append))))

(defun my-configure-all-fontsets (&optional frame size)
  "Set fallbacks for all fontsets on FRAME, with SIZE.
Chenging the size of the default font creates new fontsets;
instead, we just set the size of the font in the existing ones."
  (when (fboundp 'fontset-list)
    (mapc (apply-partially #'my-configure-one-fontset frame size) (fontset-list))))

(dolist (frame (frame-list)) (my-configure-all-fontsets frame))
(add-to-list 'after-make-frame-functions #'my-configure-all-fontsets)

(set-face-attribute 'variable-pitch nil :family "Fira Sans")

(with-eval-after-load 'which-func
  (set-face-attribute 'which-func nil :foreground 'unspecified)
  (set-face-attribute 'which-func nil :inherit font-lock-doc-face))

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
