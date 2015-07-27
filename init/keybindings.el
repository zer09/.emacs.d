;;;;;;;;;;;;;;;;;
;; Keybindings ;;
;;;;;;;;;;;;;;;;;

;; NOTE: Use where-is to find a keybinding

;; Global keybindings ;;

(when window-system
  (global-unset-key "\C-z"))

;; Help keys ;;

(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)

;; Local keybindings ;;

;; Unoverridable shortcuts from http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
(defvar keybindings-minor-mode-map (make-keymap) "keybindings-minor-mode keymap.")

(define-key keybindings-minor-mode-map [\M-up] 'move-line-up)
(define-key keybindings-minor-mode-map [\M-down] 'move-line-down)

(define-key keybindings-minor-mode-map (kbd "C-c m") 'magit-status)
(define-key keybindings-minor-mode-map (kbd "C-c o") 'find-file-here)

(define-key keybindings-minor-mode-map (kbd "C-M-;") 'comment-dwim)
(define-key keybindings-minor-mode-map (kbd "M-;") 'comment-or-uncomment-region-or-line)

;; Move quickly between windows
(define-key keybindings-minor-mode-map (kbd "C-x <right>") 'windmove-right)
(define-key keybindings-minor-mode-map (kbd "C-x <left>")  'windmove-left)
(define-key keybindings-minor-mode-map (kbd "C-x <up>")    'windmove-up)
(define-key keybindings-minor-mode-map (kbd "C-x <down>")  'windmove-down)

;; Windresize
(define-key keybindings-minor-mode-map (kbd "C-x /") 'windresize)

;; Multiple cursors
(define-key keybindings-minor-mode-map (kbd "C-S-c")   'mc/edit-lines)
(define-key keybindings-minor-mode-map (kbd "C->")     'mc/mark-next-like-this)
(define-key keybindings-minor-mode-map (kbd "C-<")     'mc/mark-previous-like-this)
(define-key keybindings-minor-mode-map (kbd "C-c C->") 'mc/mark-all-like-this)

;; Avy — quick moving to position
(define-key keybindings-minor-mode-map (kbd "C-c SPC") 'avy-goto-char)
(define-key keybindings-minor-mode-map (kbd "M-g w") 'avy-goto-word-1)
(define-key keybindings-minor-mode-map (kbd "M-g f") 'avy-goto-line)

;; smex — ido in M-x menu
(define-key keybindings-minor-mode-map (kbd "M-x") 'smex)
(define-key keybindings-minor-mode-map (kbd "C-c M-x") 'execute-extended-command) ;; old M-x

;; expand-region
(define-key keybindings-minor-mode-map (kbd "C-M-SPC") 'er/expand-region)

;; visual centering
(define-key keybindings-minor-mode-map (kbd "C-c l") 'center-visual-lines)

;; refactoring
(define-key keybindings-minor-mode-map (kbd "C-<menu>") 'emr-show-refactor-menu)

;; rgrep
(define-key keybindings-minor-mode-map (kbd "C-c s") 'ag)

;; visual-regexp
;; Broken handling of newlines: (require 'visual-regexp-steroids)
(define-key keybindings-minor-mode-map (kbd "C-c r") 'vr/query-replace)
(define-key keybindings-minor-mode-map (kbd "C-M-%") 'vr/query-replace)

;; Zoom settings
(define-key keybindings-minor-mode-map [\C-mouse-4] 'zoom-in)
(define-key keybindings-minor-mode-map [\C-mouse-5] 'zoom-out)
(define-key keybindings-minor-mode-map [\C-kp-add] 'zoom-in)
(define-key keybindings-minor-mode-map [\C-kp-subtract] 'zoom-out)
(define-key keybindings-minor-mode-map (kbd "<C-kp-0>") 'zoom-reset)

;; Get rid of left-word, right-word
(define-key keybindings-minor-mode-map [\C-left] 'backward-word)
(define-key keybindings-minor-mode-map [\C-right] 'forward-word)

;; Compilation
(define-key keybindings-minor-mode-map (kbd "C-c c") 'compile)

;; Slides
(define-key keybindings-minor-mode-map (kbd "C-M->") 'next-slide)
(define-key keybindings-minor-mode-map (kbd "C-M-<") 'prev-slide)

;; Open line and indent properly
(define-key keybindings-minor-mode-map (kbd "C-o") 'open-and-indent-next-line)
(define-key keybindings-minor-mode-map (kbd "<C-M-return>") 'open-and-indent-next-line)

;; Find recent
(define-key keybindings-minor-mode-map (kbd "C-x C-r") 'recentf-open-files)

;; Folding and unfolding
(global-set-key (kbd "C-c C-/") 'hs-hide-all)
(global-set-key (kbd "C-c C-\\") 'hs-show-all)

(define-minor-mode keybindings-minor-mode
  "Minor mode to prevent overriding key bindings."
  t " kbd" 'keybindings-minor-mode-map)

(define-globalized-minor-mode keybindings-global-mode
  keybindings-minor-mode (lambda () (keybindings-minor-mode 1)))

(add-hook 'minibuffer-setup-hook (lambda () (keybindings-minor-mode 0)))
(add-hook 'after-init-hook 'keybindings-global-mode)
(diminish 'keybindings-minor-mode)
