(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4e803b7d84cfcb0a124f74b77a198982d7b11705220da1a757dc013d9ceaff06" "926d2c3fd1aff97ce6b43e69072cb3de426393a76790c771995ec162b194c2c6" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(frame-background-mode (quote dark))
 '(package-selected-packages
   (quote
    (which-key hide-lines page-break-lines flycheck yaml-mode nameless shut-up jquery-doc js2-mode smart-mode-line avy json-mode jinja2-mode aggressive-indent emr flycheck-pos-tip highlight-symbol haskell-mode company ag markdown-mode company-math csharp-mode expand-region ido-ubiquitous julia-mode magit multiple-cursors yasnippet flycheck-package omnisharp wgrep-ag windresize wgrep diminish latex-extra ws-butler visual-regexp tuareg smex racket-mode rainbow-delimiters popup json-rpc epc elpy company-auctex)))
 '(safe-local-variable-values
   (quote
    ((TeX-command-extra-options . "-shell-escape")
     (eval progn
           (let
               ((coq-root-directory
                 (when buffer-file-name
                   (locate-dominating-file buffer-file-name ".dir-locals.el")))
                (coq-project-find-file
                 (and
                  (boundp
                   (quote coq-project-find-file))
                  coq-project-find-file)))
             (set
              (make-local-variable
               (quote tags-file-name))
              (concat coq-root-directory "TAGS"))
             (setq camldebug-command-name
                   (concat coq-root-directory "dev/ocamldebug-coq"))
             (unless coq-project-find-file
               (set
                (make-local-variable
                 (quote compile-command))
                (concat "make -C " coq-root-directory))
               (set
                (make-local-variable
                 (quote compilation-search-path))
                (cons coq-root-directory nil)))
             (when coq-project-find-file
               (setq default-directory coq-root-directory))))
     (dafny-prover-local-args "/autoTriggers:1")))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tooltip ((t (:background "systeminfowindow" :foreground "systeminfotext" :height 0.8)))))
