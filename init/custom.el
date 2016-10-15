(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("b04425cc726711a6c91e8ebc20cf5a3927160681941e06bc7900a5a5bfe1a77f" "4e803b7d84cfcb0a124f74b77a198982d7b11705220da1a757dc013d9ceaff06" "926d2c3fd1aff97ce6b43e69072cb3de426393a76790c771995ec162b194c2c6" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
 '(org-agenda-files nil)
 '(package-selected-packages
   '(list-utils flycheck-rust racer rust-mode slideview dirtree python-info python-docstring emojify alert sml-mode load-theme-buffer-local tangotango-theme writeroom-mode projectile php-mode nginx-mode font-lock-studio debbugs flatui-theme hide-lines stream visual-fill-column nyan-mode typescript-mode cider graphviz-dot-mode 0blayout rainbow-delimiters csharp-mode darkroom web-mode adaptive-wrap buttercup cask package-build epl git commander f dash s fill-column-indicator undercover bibretrieve gscholar-bibtex keyfreq org xelb flx-ido magit-gh-pulls paren-face company-quickhelp company magit git-commit with-editor tldr noflet color-theme-solarized pos-tip pyvenv which-key page-break-lines flycheck yaml-mode nameless shut-up js2-mode smart-mode-line json-mode jinja2-mode aggressive-indent highlight-symbol haskell-mode ag markdown-mode company-math expand-region ido-ubiquitous julia-mode multiple-cursors yasnippet flycheck-package wgrep-ag windresize wgrep diminish latex-extra ws-butler visual-regexp tuareg smex racket-mode popup elpy company-auctex))
 '(safe-local-variable-values
   '((python-shell-interpreter . "python2")
     (indent-tabs-mode nil)
     (company-idle-delay . 5)
     (org-hierarchical-todo-statistics)
     (org-log-done)
     (eval progn
           (let
               ((coq-root-directory
                 (when buffer-file-name
                   (locate-dominating-file buffer-file-name ".dir-locals.el")))
                (coq-project-find-file
                 (and
                  (boundp 'coq-project-find-file)
                  coq-project-find-file)))
             (setq tags-file-name
                   (concat coq-root-directory "TAGS")
                   camldebug-command-name
                   (concat coq-root-directory "dev/ocamldebug-coq"))
             (unless coq-project-find-file
               (setq compile-command
                     (concat "make -C " coq-root-directory)))
             (when coq-project-find-file
               (setq default-directory coq-root-directory))))
     (TeX-command-extra-options . "-shell-escape")
     (eval progn
           (let
               ((coq-root-directory
                 (when buffer-file-name
                   (locate-dominating-file buffer-file-name ".dir-locals.el")))
                (coq-project-find-file
                 (and
                  (boundp 'coq-project-find-file)
                  coq-project-find-file)))
             (set
              (make-local-variable 'tags-file-name)
              (concat coq-root-directory "TAGS"))
             (setq camldebug-command-name
                   (concat coq-root-directory "dev/ocamldebug-coq"))
             (unless coq-project-find-file
               (set
                (make-local-variable 'compile-command)
                (concat "make -C " coq-root-directory))
               (set
                (make-local-variable 'compilation-search-path)
                (cons coq-root-directory nil)))
             (when coq-project-find-file
               (setq default-directory coq-root-directory))))
     (dafny-prover-local-args "/autoTriggers:1"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mu4e-header-highlight-face ((t (:inherit region))))
 '(tooltip ((t (:inherit nil :background "lightyellow" :foreground "black")))))
