(defvar agda-prettify-symbols-alist
  '(;; Types
    ("::" . ?∷)

    ;; Quantifiers
    ("forall" . ?∀)
    ("exists" . ?∃)

    ;; Arrows
    ("->" . ?→)
    ("-->" . ?⟶)
    ("<-" . ?←)
    ("<--" . ?⟵)
    ("<->" . ?↔)
    ("<-->" . ?⟷)

    ("=>" . ?⇒)
    ("==>" . ?⟹)
    ("<==" . ?⟸)
    ("<=>" . ?⇔)
    ("<==>" . ?⟺)

    ("|->" . ?↦)
    ("|-->" . ?⟼)
    ("<-|" . ?↤)
    ("<--|" . ?⟻)

    ("|=>" . ?⤇)
    ("|==>" . ?⟾)
    ("<=|" . ?⤆)
    ("<==|" . ?⟽)

    ("~>" . ?⇝)
    ("<~" . ?⇜)

    (">->" . ?↣)
    ("<-<" . ?↢)
    ("->>" . ?↠)
    ("<<-" . ?↞)

    (">->>" . ?⤖)
    ("<<-<" . ?⬻)

    ("<|-" . ?⇽)
    ("-|>" . ?⇾)
    ("<|-|>" . ?⇿)

    ("<-/-" . ?↚)
    ("-/->" . ?↛)

    ("<-|-" . ?⇷)
    ("-|->" . ?⇸)
    ("<-|->" . ?⇹)

    ("<-||-" . ?⇺)
    ("-||->" . ?⇻)
    ("<-||->" . ?⇼)

    ("-o->" . ?⇴)
    ("<-o-" . ?⬰)

    ;; Boolean operators
    ("not" . ?¬)
    ("&&" . ?∧)
    ("||" . ?∨)

    ;; Relational operators
    ("==" . ?≡)
    ("/=" . ?≠)
    ("<=" . ?≤)
    (">=" . ?≥)
    ("/<" . ?≮)
    ("/>" . ?≯)

    ;; Containers / Collections
    ("++" . ?⧺)
    ("+++" . ?⧻)
    ("|||" . ?⫴)

    ;; Other
    ("<<" . ?≪)
    (">>" . ?≫)
    ("<<<" . ?⋘)
    (">>>" . ?⋙)
    ("<|" . ?⊲)
    ("|>" . ?⊳)
    ("><" . ?⋈)
    ("<*>" . ?⊛)
    (":=" . ?≔)
    ("=:" . ?≕)
    ("=def" . ?≝)
    ("=?" . ?≟)
    ("..." . ?…)))

(provide 'agda-prettify)
