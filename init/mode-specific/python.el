(when-os 'gnu/linux
  (setq-default python-shell-interpreter "python3"))

(when-os 'windows-nt
  (setq-default python-shell-interpreter "pythonw"))

(with-eval-after-load 'elpy
  (put 'pyvenv-workon 'safe-local-variable #'stringp)

  (let ((elpy-disabled '(elpy-module-highlight-indentation elpy-module-flymake)))
    (setq-default elpy-modules (cl-set-difference elpy-modules elpy-disabled)))

  (setq elpy-rpc-backend "jedi")
  (defvaralias 'elpy-rpc-python-command 'python-shell-interpreter)

  (let ((elpy-disabled-bindings '("<S-return>" "<C-S-return>" "<C-return>" "M-TAB"
                                  "<C-down>" "<C-up>" "<C-left>" "<C-right>"
                                  "<M-down>" "<M-up>" "<M-left>" "<M-right>"
                                  "C-c C-n" "C-c C-p")))
    (dolist (binding elpy-disabled-bindings)
      (define-key elpy-mode-map (kbd binding) nil))))

(defun setup-elpy ()
  "Setup elpy."
  (require 'python-prettify)
  (setq-local prettify-symbols-alist python-prettify-symbols-alist)
  (prettify-symbols-mode)

  (add-to-list 'load-path "~/.emacs.d/lisp/litpy/")
  (require 'litpy-mode)
  (litpy-mode)
  (hs-minor-mode)

  (setq-local parens-require-spaces nil)

  (let* ((buffer-path (buffer-file-name))
         (true-path   (and buffer-path (file-truename buffer-path))))
    (if (and true-path (string-match-p (regexp-opt '("afs")) true-path))
        (progn (elpy-mode -1)
               (eldoc-mode -1)
               (hs-minor-mode -1))
      (elpy-enable)
      (elpy-mode))))

(require 'flycheck)

(defvar ~/flycheck-whitelist '("~"))
(defvar ~/flycheck-blacklist '("~/downloads" "~/dls"))

(defun ~/flycheck-path-descends-p (file-true-name directory fold-case)
  "Check if FILE-TRUE-NAME is a descendant of DIRECTORY.
With FOLD-CASE, path comparisons are case-insensitive."
  (string-prefix-p (file-truename directory) file-true-name t))

(defun ~/flycheck-path-descends-any-p (file-true-name directories fold-case)
  "Check if FILE-TRUE-NAME is a descendant of any one of DIRECTORIES.
With FOLD-CASE, path comparisons are case-insensitive."
  (seq-some (lambda (dir)
              (~/flycheck-path-descends-p file-true-name dir fold-case))
            directories))

(defun ~/flycheck-verification-result-y/n (label value &optional flip)
  "Construct a verification result from LABEL and boolean VALUE.
With non-nil FLIP, return opposite of VALUE."
  (flycheck-verification-result-new
   :label label :message (if value "yes" "no")
   :face (if (if flip (not value) value) 'success 'error)))

(defun ~/flycheck-current-buffer-trusted-p (_checker)
  "Check whether current buffer contains a trusted file."
  (-if-let* ((fname (buffer-file-name))
             (f-true-name (file-truename fname)))
      (let ((whitelisted (~/flycheck-path-descends-any-p
                          fname ~/flycheck-whitelist t))
            (blacklisted (~/flycheck-path-descends-any-p
                          fname ~/flycheck-blacklist nil)))
        (list
         (~/flycheck-verification-result-y/n "whitelisted" whitelisted)
         (~/flycheck-verification-result-y/n "blacklisted" blacklisted t)))
    (flycheck-verification-result-new
     :label "Buffer file name" :message "Unspecified" :face 'error)))

(flycheck-def-option-var flycheck-python-mypy-use-python-2 nil (python-mypy)
  "Whether to pass --py2 to mypy."
  :type 'boolean
  :safe #'booleanp
  :package-version '("flycheck" . "30"))

(flycheck-def-option-var flycheck-python-mypy-silent-imports nil (python-mypy)
  "Whether to disable type-checking of imported modules."
  :type 'boolean
  :safe #'booleanp
  :package-version '("flycheck" . "30"))

(flycheck-define-checker python-mypy
  "A Python type checker using mypy.

See URL `http://www.mypy-lang.org/'."
  :command ("mypy"
            "--shadow-file" source-original source
            (option-flag "--py2" flycheck-python-mypy-use-python-2)
            (option-flag "--silent-imports" flycheck-python-mypy-silent-imports)
            "--fast-parser"
            "--check-untyped-defs"
            "--warn-redundant-casts"
            "--warn-unused-ignores"
            "--suppress-error-context"
            source-original)
  :error-patterns
  ((error line-start (file-name) ":" line ":" (optional column ":")
          " error:" (message) line-end))
  :next-checkers (python-pylint)
  :modes python-mode)

(flycheck-define-checker python-doctest
  "A Python checker for doctests."
  :command ("python" (eval (expand-file-name "~/.emacs.d/init/mode-specific/flycheck-python-doctest.py"))
            "--original-file-name" source-original)
  :standard-input t
  :error-parser flycheck-parse-checkstyle
  :verify ~/flycheck-current-buffer-trusted-p
  :modes python-mode)

;; HACK HACK HACK this ensures that flycheck runs the right python instance
(defvaralias 'flycheck-python-pylint-executable 'python-shell-interpreter)
(defvaralias 'flycheck-python-flake8-executable 'python-shell-interpreter)
(defvaralias 'flycheck-python-doctest-executable 'python-shell-interpreter)

(add-to-list 'flycheck-checkers 'python-doctest t)
(flycheck-add-next-checker 'python-pylint '(warning . python-doctest))

(add-hook 'python-mode-hook #'setup-elpy)

(define-minor-mode 6009-python-mode
  "Python mode for 6.009 tutorials."
  :lighter " 6.009"
  (when 6009-python-mode
    (flycheck-mode -1)
    (flyspell-mode -1)
    (prettify-symbols-mode -1)
    (setq-local fill-column 64)
    (setq-local python-shell-interpreter "python2.7")
    (setq-local flycheck-disabled-checkers '(python-mypy))
    (setq-local presenter-slide-separator "################################################################")
    (setq-local presenter-centered-lines-delimiters '(("##@" . "") ("#@" . "")))
    (setq-local presenter-hidden-block-delimiters
                '(("# -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*- skip this -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-\n" .
                   "\n# -*-*-*-*-*-*-*-*-*-*-*-*-*-  resume reading here  -*-*-*-*-*-*-*-*-*-*-*-*-*-")))
    (add-to-list 'font-lock-extra-managed-props 'display)
    (define-key python-mode-map (kbd "<f9>") #'flycheck-mode)))

(defun ~/6009-maybe ()
  "Enable 6.009 mode for files in mit/6.009 folder."
  (when (and buffer-file-name
             (string-prefix-p (file-truename "~/documents/mit/6.009")
                              (file-truename buffer-file-name)))
    (6009-python-mode)))

(add-hook 'python-mode-hook #'~/6009-maybe t)
