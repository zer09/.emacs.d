;;;;;;;;;;;;;;;;;;;
;; Font settings ;;
;;;;;;;;;;;;;;;;;;;

(defun my-set-font-fallbacks (fontset)
  "Add fallbacks to FONTSET."
  (let ((cjk-font (font-spec :name "WenQuanYi Micro Hei Mono"))
        (size     (face-attribute 'default :height)))
    (set-fontset-font fontset 'unicode (font-spec :name "Consolas") nil)
    ;; (set-fontset-font fontset (cons ?∀ ?∀) "Symbola" nil 'prepend)
    ;; (set-fontset-font fontset (cons ?≜ ?≜) "XITS Math" nil 'prepend)
    ;; (set-fontset-font fontset (cons ?⇒ ?⇒) "DejaVu Sans Mono" nil 'prepend)
    ;; (set-fontset-font fontset (cons ?≔ ?≔) "FreeSerif" nil 'prepend)
    ;; (set-fontset-font fontset 'unicode (font-spec :name "FreeMono") nil 'append) ; :size (* 0.12 size)
    (set-fontset-font fontset 'unicode (font-spec :name "Segoe UI Emoji") nil 'append) ;; '(#x1F600 . #x1F64F)
    (set-fontset-font fontset 'unicode (font-spec :name "Symbola monospacified for Consolas") nil 'append)
    ;; (set-fontset-font fontset 'unicode (font-spec :name "Asana Math monospacified for Consolas") nil 'append)
    ;; (set-fontset-font fontset 'unicode (font-spec :name "Tex Gyre Schola Math monospacified for Consolas") nil 'append)
    ;; (set-fontset-font fontset 'unicode (font-spec :name "Latin Modern Math monospacified for Consolas") nil 'append)
    ;; (set-fontset-font fontset 'unicode (font-spec :name "XITS Math monospacified for Consolas") nil 'append)
    (set-fontset-font fontset '(#x4E00 . #x9FFF) cjk-font nil 'append)
    (set-fontset-font fontset '(#x3400 . #x4DFF) cjk-font nil 'append)
    (set-fontset-font fontset '(#x20000 . #x2A6DF) cjk-font nil 'append)
    (set-fontset-font fontset '(#xF900 . #xFAFF) cjk-font nil 'append)
    (set-fontset-font fontset '(#x2F800 . #x2FA1F) cjk-font nil 'append)))

(defun my-reset-all-font-fallbacks ()
  "Set fallbacks for all fontsets.
This is needed as scaling creates new fontsets, and just setting
fontset-default or fontset-startup doesn't solve the scaling
problem."
  (when (and window-system (fboundp 'fontset-list))
    (mapc #'my-set-font-fallbacks (fontset-list))))

(my-reset-all-font-fallbacks)

;; Font selection ;;
;; Faster than custom-set-faces
;; But still slower than using .Xresources
;; ;; (set-face-attribute 'default nil
;; ;;                     :inherit nil
;; ;;                     :background "#ffffff"
;; ;;                     :foreground "#000000"
;; ;;                     :height 98
;; ;;                     :family "Consolas")
;; Replaced by .Xresources containing "Emacs.font: Consolas-10"
