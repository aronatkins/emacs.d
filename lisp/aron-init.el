
;;; aron-init.el --- Personal configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; Personal Emacs settings and preferences.

;;; Code:

;; Lots of good config examples:
;; http://www.djcbsoftware.nl/dot-emacs.html

(require 'aron-treesitter)
(require 'aron-func)
(require 'aron-grep)
(require 'aron-keys)
(require 'aron-compile)

;; a theme to this story
;; https://emacsthemes.com
(use-package leuven-theme
  :ensure t
  :custom
  ;; disable scaling of text in things like Markdown files
  (leuven-scale-outline-headlines nil)
  (leuven-scale-org-agenda-structure nil)
  (leuven-scale-volatile-highlight nil)
  :config
  (load-theme 'leuven t))

;; Mode line indicators
(line-number-mode t)
(column-number-mode t)

;; Disable information about *scratch*
(setopt initial-scratch-message nil)

;; Suppress native compilation warnings popup (still logged to *Messages*)
(setopt native-comp-async-report-warnings-errors 'silent)

;; trailing whitespace is nice, but it also highlights lines with
;; only-whitespace (ie. empty lines in a code block that happen to be
;; indented).
;; (setopt show-trailing-whitespace t)

(setopt font-lock-maximum-decoration t)

;; emacs (pre-23.1) used to use completion-ignore-case for both
;; find-file completion and buffer-switching completion.
(setopt completion-ignore-case t)
(setopt read-file-name-completion-ignore-case t)
(setopt read-buffer-completion-ignore-case t)

;; Make fill mode accept ". " as a sentence end.
(setopt sentence-end-double-space nil)

;; reasonable max width for filled regions.
(setopt fill-column 78)

;; show unfinished keystrokes early.
(setopt echo-keystrokes 0.1)

;; Visual feedback on selections
(setopt transient-mark-mode t)

;; Always end a file with a newline
(setopt require-final-newline t)

;; Stop at the end of the file, not just add lines
(setopt next-line-add-newlines nil)

;; Do not insert tabs.
(setopt indent-tabs-mode nil)

;; highlight during query
(setopt query-replace-highlight t)
;; incremental search highlights
(setopt search-highlight t)
;; stop L/R window splitting
(setopt split-width-threshold nil)

;; macOS: Install aspell with homebrew.
;; brew install aspell
(setopt ispell-program-name "aspell")

;; ask me before death. Command-q is an accident!
(setopt confirm-kill-emacs #'y-or-n-p)

;; control-L behavior. http://irreal.org/blog/?p=6436
;; also. try out C-M-l !!!
;; (setopt recenter-positions '(top middle bottom))

;; Dynamic GC management - high threshold during activity, GC when idle
(use-package gcmh
  :ensure t
  :config
  (gcmh-mode 1))

;; https://www.emacswiki.org/emacs/AlarmBell
(setopt ring-bell-function 'ignore)

(use-package lua-mode
  :ensure t
  :custom
  (lua-indent-level 2))

;;(setq auto-compression-mode t)          ;; auto-handle .gz and .Z files
(auto-compression-mode t)

;; Enable commands disabled by default.
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
;; (put 'downcase-region 'disabled nil) ;; What's this do?

; Setting this variable will cause the compile buffer to always stay at the end.
(setopt compilation-scroll-output t)
;; avoid most compilation-line truncation.
(setopt compilation-max-output-line-length 4000)
;; compilation-spawned shells are "interactive", meaning we get .bashrc
;; https://stackoverflow.com/a/17595062
(define-advice compile (:around (orig-fun &rest args) use-bashrc)
  "Load .bashrc in any calls to bash (e.g. so we can use aliases)"
  (let ((shell-command-switch "-ic"))
    (apply orig-fun args)))

; symmetric scroll up/down. http://irreal.org/blog/?p=3963
(setopt scroll-preserve-screen-position 'always)

;; make cursor the width of the character it is under
;; i.e. full width of a TAB
;; (setq x-stretch-cursor t)
;; doesn't interact well with indent-guide.

;; Files to auto-revert when reloaded.
; (setq revert-without-query '(".*\.[ch]"))

;; (setq enable-recursive-minibuffers t)
;; (setq version-control nil)           ; numbered backups for files which have them

;; completion ---
;; https://www.masteringemacs.org/article/introduction-to-ido-mode
;; display any item that contains the typed chars .. quite a shift
(use-package ido
  :custom
  (ido-enable-flex-matching t)
  (ido-everywhere t)
  ;; (ido-create-new-buffer 'always)
  :config
  (ido-mode t))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (defalias 'list-buffers 'ibuffer))

;; uniquify: buffer names are uniquified with parts of the file path.
(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'post-forward)
  (uniquify-after-kill-buffer-p t))

(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode t))

;; consider disabling auto-fill for markdown files.
;; use visual-fill-mode and consider using visual-fill-column to control the wrap width according to fill-column.
;; https://melpa.org/#/visual-fill-column https://codeberg.org/joostkremers/visual-fill-column
(use-package text-mode
  :hook ((text-mode . turn-on-auto-fill)
         (text-mode . flyspell-mode)))

(use-package prog-mode
  :hook (prog-mode . flyspell-prog-mode))

;; To use c++-mode for .h files in a specific project, add to .dir-locals.el:
;; ((c-mode . ((mode . c++))))

;; make some common keywords stand out.
;; found on: http://emacs-fu.blogspot.com/2008/12/highlighting-todo-fixme-and-friends.html
(defvar fixme-and-friends
  '(("\\<\\(FIXME\\|TODO\\|NYI\\|TBD\\|BUG\\|XXX\\):" 1 font-lock-warning-face t)))
(font-lock-add-keywords 'python-mode fixme-and-friends)

(use-package comint
  :custom
  (comint-completion-addsuffix '("/" . " "))
  ;; Match password prompts for comint-watch-for-password-prompt.
  (comint-password-prompt-regexp
   "\\(\\([Oo]ld \\|[Nn]ew \\|Kerberos \\|'s \\|login \\|CVS \\|^\\)[Pp]assword\\( (again)\\)?\\|pass ?phrase\\|Enter passphrase\\)\\( for \\(RSA key \\)?[^@ \t\n]+\\(@[^@ \t\n]+\\)?\\)?\\(, try again\\)?:\\s *\\'")
  :config
  (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt))

(defun aron/shell-mode-setup ()
  "Setup for shell-mode."
  ;; https://www.johndcook.com/blog/2016/11/30/setting-up-emacs-shell-on-a-mac/
  (local-set-key (kbd "<M-up>") 'comint-previous-input)
  (local-set-key (kbd "<M-down>") 'comint-next-input))

(use-package shell
  :hook (shell-mode . aron/shell-mode-setup))

(use-package json-mode
  :ensure t
  :mode ("\\.eslintrc\\'" "\\.json.erb\\'"))

(use-package add-node-modules-path
  :ensure t
  :commands (add-node-modules-path))

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(use-package js
  :bind (:map js-mode-map ("M-." . nil))
  :custom
  (js-indent-level 2))

(defun aron/js2-mode-setup ()
  "Setup for js2-mode."
  (add-node-modules-path)
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)
  (add-hook 'after-save-hook #'aron/eslint-fix-file-and-revert nil t))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :hook (js2-mode . aron/js2-mode-setup)
  :custom
  (js2-strict-trailing-comma-warning nil))

(defun aron/vue-mode-setup ()
  "Setup for vue-mode."
  (add-node-modules-path)
  (add-hook 'after-save-hook #'aron/eslint-fix-file-and-revert nil t))

(defun aron/mmm-fix-syntax-ppss ()
  "Fix syntax parsing in mmm submodes."
  (setq syntax-ppss-table nil))

(use-package vue-mode
  :ensure t
  :hook (vue-mode . aron/vue-mode-setup)
  :config
  ;; suppress the region background color
  (setopt mmm-submode-decoration-level 0)
  ;; fix bad indents in vue JS blocks
  ;; https://github.com/AdamNiederer/vue-mode/issues/74
  ;; https://github.com/AdamNiederer/vue-mode/issues/100
  (add-hook 'mmm-js-mode-enter-hook #'aron/mmm-fix-syntax-ppss)
  (add-hook 'mmm-typescript-mode-enter-hook #'aron/mmm-fix-syntax-ppss))

(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode)
  :custom
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (flycheck-emacs-lisp-load-path load-path)
  :config
  ;; Disable flycheck on indirect buffers (e.g., quarto-mode+polymode)
  (defun flycheck-buffer-not-indirect-p (&rest _)
    "Ensure that the current buffer is not indirect."
    (null (buffer-base-buffer)))
  (advice-add 'flycheck-may-check-automatically
              :before-while #'flycheck-buffer-not-indirect-p))

(use-package html-ts-mode
  :mode "\\.html\\'"
  :init
  (aron/ensure-treesit-grammar 'html))

;; might need https://github.com/editorconfig/editorconfig-emacs#customize
;; to get web-mode to play nicely with editorconfig.
;(setq web-mode-markup-indent-offset 2)
;(setq web-mode-attr-indent-offset 4)
;(setq web-mode-code-indent-offset 2)
;(setq web-mode-script-padding 2)

;; (setopt nxml-child-indent 4)
;; (setopt js-indent-level 2)  ; used by js-mode, json-mode
;; (setopt js2-basic-offset 2) ; used by js2-mode

;; Markdown / RMarkdown

;; gfm-mode is a GitHub-flavored markdown mode (part of markdown-mode package)
(use-package markdown-mode
  :ensure t
  :mode (("\\.markdown\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.Rmd\\'" . gfm-mode)
         ("\\.Rmd\\.tmpl\\'" . gfm-mode))
  :custom
  (markdown-gfm-use-electric-backquote nil))

(use-package poly-markdown
  :ensure t
  :mode "\\.qmd\\'")

;; SQL
(use-package sql-indent
  :ensure t
  :hook (sql-mode . sqlind-minor-mode))

;; https://github.com/zk-phi/indent-guide
(use-package indent-guide
  :ensure t
  :custom-face
  (indent-guide-face ((t (:foreground "gray"))))
  :config
  (indent-guide-global-mode))

;; http://www.lunaryorn.com/2014/09/13/boosting-which-func-mode.html
(use-package which-func
  :config
  (which-function-mode))

;; https://github.com/TeMPOraL/nyan-mode
;;(nyan-mode)

(defun aron/ess-r-mode-setup ()
  "Setup for ess-r-mode."
  (setq tab-width 2)
  ;; Use .lintr configuration rather than emacs default.
  (setq-local flycheck-lintr-linters "NULL")
  ;; Format on save via eglot.
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))

(use-package ess-r-mode
  :ensure ess
  :hook ((ess-r-mode . aron/ess-r-mode-setup)
         (ess-r-mode . eglot-ensure))
  :custom
  (ess-indent-offset 2)
  (ess-use-flymake nil)
  :config
  (ess-set-style 'RStudio)
;; brew install air
(if (executable-find "air")
    (add-to-list 'eglot-server-programs
                 '((R-mode ess-r-mode) . ("air" "language-server")))
  (warn "R language server not found: air")))

(use-package jenkinsfile-mode
  :ensure t
  :mode "Jenkinsfile.*")

(use-package yaml-ts-mode
  :mode ("\\.yml\\'" "\\.yaml\\'" "\\.yaml\\.gotmpl\\'")
  :init
  (aron/ensure-treesit-grammar 'yaml))

(use-package just-ts-mode
  :ensure t
  :mode ("justfile\\'" "\\.just\\'")
  :init
  (aron/ensure-treesit-grammar 'just))

;; Per-project eglot server config via .dir-locals.el:
;; ((templ-ts-mode
;;   . ((eval . (setf (alist-get 'templ-ts-mode eglot-server-programs)
;;                    '("go" "tool" "templ" "lsp"))))))
(use-package templ-ts-mode
  :ensure t
  :after go-ts-mode
  :mode "\\.templ\\'"
  :init
  (aron/ensure-treesit-grammar 'javascript)
  (aron/ensure-treesit-grammar 'templ)
  :hook (templ-ts-mode . eglot-ensure)
  :config
  (add-to-list 'safe-local-eval-forms
               '(setf (alist-get 'templ-ts-mode eglot-server-programs)
                      '("go" "tool" "templ" "lsp"))))

;; gcfg isn't quite gitconfig, but it's close.
;; https://code.google.com/p/gcfg/
(use-package gcfg-mode
  :mode "\\.gcfg\\'")

(use-package python
  :mode ("\\.py\\'" . python-ts-mode)
  :init
  (aron/ensure-treesit-grammar 'python)
  :hook (python-ts-mode . eglot-ensure)
  :custom
  (python-fill-docstring-style 'django)
  :config
  (if (file-executable-p (expand-file-name "~/python/env/bin/pylsp"))
      (add-to-list 'eglot-server-programs
                   '((python-mode python-ts-mode) . ("~/python/env/bin/pylsp")))
    (warn "Python language server not found: ~/python/env/bin/pylsp")))

;; Go
;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
;; https://github.com/joaotavora/eglot/issues/574
(use-package project
  :config
  ;; Custom project detection for Go modules (finds go.mod)
  (defun project-find-go-module (dir)
    (when-let ((root (locate-dominating-file dir "go.mod")))
      (cons 'go-module root)))
  (cl-defmethod project-root ((project (head go-module)))
    (cdr project))
  (add-hook 'project-find-functions #'project-find-go-module))

;; note: https://github.com/weijiangan/flycheck-golangci-lint/issues/24
;; note: https://github.com/weijiangan/flycheck-golangci-lint/issues/28
;;
;; For "go tool golangci-lint", create a wrapper script and set via .dir-locals.el:
;; ((go-ts-mode . ((flycheck-golangci-lint-executable . "/path/to/wrapper"))))
(use-package flycheck-golangci-lint
  :ensure t
  :hook (go-ts-mode . flycheck-golangci-lint-setup)
  :config
  ;; Fix version detection to use flycheck-checker-executable (issue #28)
  (defun flycheck-golangci-lint--parse-version ()
    "Parse golangci-lint version from --version output."
    (unless flycheck-golangci-lint--version
      (let* ((output (ignore-errors
                       (with-temp-buffer
                         (call-process (flycheck-checker-executable 'golangci-lint)
                                       nil t nil "--version")
                         (buffer-string))))
             (version-regex "version \\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)"))
        (when (and output (string-match version-regex output))
          (setq flycheck-golangci-lint--version
                (list (string-to-number (match-string 1 output))
                      (string-to-number (match-string 2 output))
                      (string-to-number (match-string 3 output)))))))
    flycheck-golangci-lint--version))

;; (setenv "GOPRIVATE" "github.com/rstudio,connect,linkwalk,envmanager,rsc-quarto,rsc-session")

;; src/connect/.dir-locals.el
;; ((nil (eglot-workspace-configuration
;;        . ((gopls
;;            . (
;;               ;; https://github.com/golang/tools/blob/master/gopls/doc/settings.md#local-string
;;               (local . "connect")
;;               ;; https://github.com/golang/tools/blob/master/gopls/doc/settings.md#staticcheck-bool
;;               (staticcheck . t)
;;               )
;;            ))
;;        )))

(use-package go-ts-mode
  :mode "\\.go\\'"
  :bind (
         ;; ("C-c i" . go-goto-imports)
         ("C-c C-c" . aron/go-compile)
         ;; ("C-c C-s" . aron/go-start) ;; BROKEN
         ("C-c C-t" . aron/go-test)
         )
  :preface
  ;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md#organizing-imports-with-eglot
  (defun aron/eglot-before-save-go ()
    (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
    (add-hook 'before-save-hook
              (lambda ()
                (call-interactively 'eglot-code-action-organize-imports))
              -9 t))
  :init
  (aron/ensure-treesit-grammar 'go)
  :hook ((go-ts-mode . eglot-ensure)
         (go-ts-mode . aron/eglot-before-save-go)))

(use-package go-mod-ts-mode
  :mode "go\\.mod\\'"
  :init
  (aron/ensure-treesit-grammar 'gomod))

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  :config
  ;; Optionally enable completion-as-you-type behavior.
  ;; (setq company-idle-delay 0)
  ;; (setq company-minimum-prefix-length 1)
  )

;; Optional - provides snippet support.
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-ts-mode . yas-minor-mode))

(use-package cmake-font-lock
  :ensure t
  :hook (cmake-mode . cmake-font-lock-activate))

;; http://pragmaticemacs.com/emacs/get-pop-up-help-for-keybindings-with-which-key/
(use-package which-key
  :ensure t
  :custom
  (which-key-idle-delay 0.5)
  :config
  (which-key-mode))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; (setopt safe-local-variable-values
;;         '(
;;           (js2-basic-offset . 2)
;;           )
;;         )

;; super awesome window movement. on the mac: command-arrow.
(use-package windmove
  :custom
  (windmove-wrap-around t)
  :config
  (windmove-default-keybindings 'super))

(use-package git-modes
  :ensure t
  ;; Expand from just .gitignore because .dotfiles repo has .gitignore_global;
  ;; it is not an ignore file for that repository.
  :mode ("\\.gitignore.*" . gitignore-mode))

(use-package winner
  :config
  (winner-mode 1))

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1))

;; https://github.com/jacktasia/dumb-jump uses:
;; git grep
;; https://github.com/ggreer/the_silver_searcher
;; https://github.com/BurntSushi/ripgrep
(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; Automatically executable scripts
;; https://emacsredux.com/blog/2021/09/29/make-script-files-executable-automatically/
(use-package executable
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

;; get compilation buffers to support color output (because no one looks at TERM)
;; https://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package whole-line-or-region
  :ensure t
  :config
  (whole-line-or-region-global-mode))

(defun aron/hcl-mode-setup ()
  "Setup for hcl-mode."
  (add-hook 'after-save-hook #'aron/hcl-fix-file-and-revert nil t))

(use-package hcl-mode
  :ensure t
  :hook (hcl-mode . aron/hcl-mode-setup))

(use-package deadgrep
  :ensure t
  :bind ("C-c g" . deadgrep))

(provide 'aron-init)
;;; aron-init.el ends here
