;;;;;;;;;;;;;;;;;
;; Keybindings ;;
;;;;;;;;;;;;;;;;;

;; NOTE: Use where-is to find a keybinding

;; Global keybindings ;;

(when (display-graphic-p)
  (global-unset-key "\C-z"))

;; Help keys ;;

(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)

;; Overridable keys

(global-set-key (kbd "C-c t") (lambda () (interactive) (find-file "~/todos.d/TODO.org")))

;; Local keybindings ;;

;; Unoverridable shortcuts from http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
(defvar keybindings-minor-mode-map (make-keymap))

(define-key keybindings-minor-mode-map (kbd "C-x k") #'kill-this-buffer)
(define-key keybindings-minor-mode-map (kbd "C-x K") #'kill-buffer)

(define-key keybindings-minor-mode-map (kbd "<M-up>") #'move-line-up)
(define-key keybindings-minor-mode-map (kbd "<M-down>") #'move-line-down)

(define-key keybindings-minor-mode-map (kbd "<C-return>") '~/company-manual-begin)

(define-key keybindings-minor-mode-map (kbd "C-c m") 'magit-status)
(define-key keybindings-minor-mode-map (kbd "C-c o") 'find-file-here)

(define-key keybindings-minor-mode-map (kbd "C-M-;") 'comment-dwim)
(define-key keybindings-minor-mode-map (kbd "M-;") 'comment-or-uncomment-region-or-line)

;; Killing emacs
(define-key keybindings-minor-mode-map (kbd "C-x C-c") #'kill-frame-or-emacs)

;; Move quickly between windows
(define-key keybindings-minor-mode-map (kbd "C-x <right>") 'windmove-right)
(define-key keybindings-minor-mode-map (kbd "C-x <left>")  'windmove-left)
(define-key keybindings-minor-mode-map (kbd "C-x <up>")    'windmove-up)
(define-key keybindings-minor-mode-map (kbd "C-x <down>")  'windmove-down)
(define-key keybindings-minor-mode-map (kbd "C-x C-<right>") 'windmove-right)
(define-key keybindings-minor-mode-map (kbd "C-x C-<left>")  'windmove-left)
(define-key keybindings-minor-mode-map (kbd "C-x C-<up>")    'windmove-up)
(define-key keybindings-minor-mode-map (kbd "C-x C-<down>")  'windmove-down)

;; IBuffer
(define-key keybindings-minor-mode-map (kbd "C-x C-b") 'ibuffer)

;; Windresize
(define-key keybindings-minor-mode-map (kbd "C-x /") 'windresize)

;; Multiple cursors
(define-key keybindings-minor-mode-map (kbd "C-S-c")   'mc/edit-lines)
(define-key keybindings-minor-mode-map (kbd "C->")     'mc/mark-next-like-this)
(define-key keybindings-minor-mode-map (kbd "C-<")     'mc/mark-previous-like-this)
(define-key keybindings-minor-mode-map (kbd "C-c C->") 'mc/mark-all-like-this)

(with-eval-after-load 'multiple-cursors-core
  (define-key mc/keymap (kbd "M-I") 'mc/insert-numbers)
  (define-key mc/keymap (kbd "M-T") 'mc/reverse-regions)
  (define-key mc/keymap (kbd "C-,") 'mc/unmark-next-like-this)
  (define-key mc/keymap (kbd "C-.") 'mc/skip-to-next-like-this)
  (define-key mc/keymap (kbd "C-.") 'mc/skip-to-next-like-this)
  ;; This one is great:
  (define-key mc/keymap (kbd "C-'") 'mc-hide-unmatched-lines-mode))

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
(define-key keybindings-minor-mode-map (kbd "C-<apps>") 'emr-show-refactor-menu)

;; rgrep
(define-key keybindings-minor-mode-map (kbd "C-c s") 'ag)
(define-key keybindings-minor-mode-map (kbd "C-c M-s") 'ag-regexp)

;; Quoting
(define-key keybindings-minor-mode-map (kbd "M-\"") 'quote-region)

;; find
(define-key keybindings-minor-mode-map (kbd "C-c f") 'find-dired)

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
(define-key keybindings-minor-mode-map (kbd "C-c c") '~/compile)

;; Slides
(define-key keybindings-minor-mode-map (kbd "C-M->") 'next-slide)
(define-key keybindings-minor-mode-map (kbd "C-M-<") 'prev-slide)

;; Open line and indent properly
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

(defadvice load (after override-other-minor-modes)
  "Ensure that keybindings-minor-mode always gets priority."
  (unless (eq (caar minor-mode-map-alist) 'my-keys-minor-mode)
    (let ((my-bindings (assq 'keybindings-minor-mode minor-mode-map-alist)))
      (assq-delete-all 'keybindings-minor-mode minor-mode-map-alist)
      (add-to-list 'minor-mode-map-alist my-bindings))))
(ad-activate 'load)

;;; Unify keybindings for various occuredit-like modes
(define-key occur-mode-map (kbd "C-c C-p") #'occur-edit-mode)
(define-key dired-mode-map (kbd "C-c C-p") #'dired-toggle-read-only)
