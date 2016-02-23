(defconst mu4e-symbols-override
  '((mu4e-headers-draft-mark     . ("D" . "📝 ")) ;; ✒ ✏
    (mu4e-headers-flagged-mark   . ("F" . "🏴 "))
    (mu4e-headers-new-mark       . ("N" . ""))
    (mu4e-headers-passed-mark    . ("P" . "→ ")) ;; ↪
    (mu4e-headers-replied-mark   . ("R" . "← "))
    (mu4e-headers-seen-mark      . ("S" . "")) ;; ✓ 🗸 ✔
    (mu4e-headers-trashed-mark   . ("T" . "✗ ")) ;; 🗑
    (mu4e-headers-attach-mark    . ("a" . "📎 "))
    (mu4e-headers-encrypted-mark . ("x" . "🔐 "))
    (mu4e-headers-signed-mark    . ("s" . "🔏 "))
    (mu4e-headers-unread-mark    . ("u" . "● ")) ;; ★
    (mu4e-headers-has-child-prefix    . ("+" . "◼"))
    (mu4e-headers-empty-parent-prefix . ("-" . "◽"))
    (mu4e-headers-first-child-prefix  . ("\\" . "↳"))
    (mu4e-headers-duplicate-prefix    . ("=" . "⚌"))
    (mu4e-headers-default-prefix      . ("|" . "┃"))))

(defun mu4e~headers-field-set-subject-face (msg field val _width)
  "Set face of VAL is FIELD is :subject."
  (when (eq field :subject)
    (add-face-text-property 0 (length val) '(:inherit variable-pitch) nil val)
    (mu4e~headers-line-apply-flag-face msg val))
  val)

(with-eval-after-load 'mu4e
  (set-face-attribute 'mu4e-unread-face nil :inherit 'default :weight 'bold :underline nil)
  (setq-default mu4e-headers-visible-lines 15
                mu4e-maildir "/home/clement/Feeds"
                mu4e-use-fancy-chars t
                mu4e-headers-fields '((:human-date . 10)
                                      (:flags . 6)
                                      (:mailing-list . 25)
                                      (:from . 15)
                                      (:subject)))
  (setq mu4e~headers-line-handler-functions nil)
  (add-to-list 'mu4e~headers-field-handler-functions #'mu4e~headers-field-set-subject-face t)
  (add-hook 'mu4e-view-mode-hook #'hide-trailing-whitespace)
  (cl-loop for (var . (letter . symbol)) in mu4e-symbols-override
           do (set var (cons letter (propertize symbol 'face '(:weight normal)))))) ;;
