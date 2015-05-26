;;;;;;;;;;;;;;;;;;;
;; Font settings ;;
;;;;;;;;;;;;;;;;;;;

;; Chinese characters fallbacks ;;

(let ((chinese-font "WenQuanYi Micro Hei Mono")
      (fontset t)) ; "fontset-default"
  (set-fontset-font fontset 'unicode (font-spec :name "Symbola") nil 'append)
  (set-fontset-font fontset '(#x4E00 . #x9FFF) (font-spec :name chinese-font))
  (set-fontset-font fontset '(#x3400 . #x4DFF) (font-spec :name chinese-font))
  (set-fontset-font fontset '(#x20000 . #x2A6DF) (font-spec :name chinese-font))
  (set-fontset-font fontset '(#xF900 . #xFAFF) (font-spec :name chinese-font))
  (set-fontset-font fontset '(#x2F800 . #x2FA1F) (font-spec :name chinese-font)))
  ;; (set-fontset-font fontset 'greek (font-spec :name "Consolas")) ;; useless if 'append is added to Symbola
  ;; (set-fontset-font fontset '(#x1F600 . #x1F64F) (font-spec :name "Segoe UI Emoji"))

;; «»‹›“”‘’〖〗【】「」『』〈〉《》〔〕ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ
;; αβγδεζηθικλμνξοπρςτυφχψω ¤$¢€₠£¥ ©®™²³ §¶†‡※ •◦‣✓●■◆○□◇★☆♠♣♥♦♤♧♡♢
;; ᴁᴂᴈ ♩♪♫♬♭♮♯ “” ‘’ ¿¡ ¶§ª - ‐ ‑ ‒ – — ― … ° ⌈⌉ ⌊⌋ ∏∑∫ ×÷ ⊕⊖⊗⊘⊙⊚⊛∙∘
;; ′″‴ ∼∂√ ≔× ⁱ⁰¹²³ ₀₁₂ π∞± ∎ ∀¬∧∨∃⊦∵∴∅∈∉⊂⊃⊆⊇⊄⋂⋃ ≠≤≥≮≯≫≪≈≡ ℕℤℚℝℂ
;; ←→↑↓ ↔ ↖↗↙↘ ⇐⇒⇑⇓ ⇔⇗ ⇦⇨⇧⇩ ↞↠↟↡ ↺↻ ☞☜☝☟ ⌘⌥‸ ⇧⌤↑↓→←⇞⇟↖↘ ⌫ ⌦ ⎋⏏↶↷◀▶▲▼
;; ◁▷△▽ ⇄ ⇤ ⇥ ↹↵↩⏎ ⌧⌨␣ ⌶ ⎗⎘⎙⎚⌚⌛ ✂✄✉✍ ①②③④⑤⑥⑦⑧⑨⓪ 卐卍
;; ✝✚✡☥⎈☭☪☮☺☹☯☰☱☲☳☴☵☶☷☠☢☣☤♲♳⌬♨♿☉☼☾☽♀♂♔♕♖♗♘♙♚♛♜♝♞♟❦　、。！，：林花謝
;; 了春紅，太匆匆。無奈朝來寒雨，晚來風。胭脂淚，留人醉，幾時重，自
;; 是人生長恨，水長東。

;; DejaVu Sans Mono
;; Symbola
;; FreeMono
;; STIX
;; Unifont
;; Segoe UI Symbol
;; Cambria Math

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
