;;; package ---  What Aron likes.

;;; Commentary:
;;; You know.

;;; Code:

;; Lots of good config examples:
;; http://www.djcbsoftware.nl/dot-emacs.html

(require 'use-package)
(require 'gcfg-mode)
(require 'aron-func)
(require 'aron-grep)
(require 'aron-keys)
(require 'aron-compile)

(use-package eglot)


;; Disable the emacs startup message.
(setopt inhibit-startup-message t)
;; Disable information about *scratch*
(setopt initial-scratch-message nil)

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

(setopt windmove-wrap-around t)

;; ask me before death. Command-q is an accident!
(setopt confirm-kill-emacs #'y-or-n-p)

;; quickly help an old man.
(setopt which-key-idle-delay 0.5)

;; control-L behavior. http://irreal.org/blog/?p=6436
;; also. try out C-M-l !!!
;; (setopt recenter-positions '(top middle bottom))

;; stop gfm (markdown) mode from having electric backticks.
(setopt markdown-gfm-use-electric-backquote nil)

;; tuning for LSP (https://emacs-lsp.github.io/lsp-mode/page/performance/#tuning)
(setopt gc-cons-threshold 100000000)
(setopt read-process-output-max (* 1024 1024)) ;; 1mb

;; https://www.emacswiki.org/emacs/AlarmBell
(setopt ring-bell-function 'ignore)

(setopt lua-indent-level 2)

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

;; (require 'icomplete)        ; active minibuffer completion
;; (setq completion-styles '(flex))
;; (setq completion-styles '(substring))
;; (icomplete-mode)
;; (fido-mode)

(defalias 'list-buffers 'ibuffer)       ; A richer list-buffers experience.

;; uniquify: buffer names are uniquified with parts of the file path.
(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'post-forward)
  (uniquify-after-kill-buffer-p t))

;;(setq auto-compression-mode t)          ;; auto-handle .gz and .Z files
(auto-compression-mode t)

(put 'narrow-to-region 'disabled nil)

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

;; http://pragmaticemacs.com/emacs/volatile-highlights/
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; fill all text. spell all text.
(add-hook 'text-mode-hook 'turn-on-auto-fill)
;; consider disabling auto-fill for markdown files.
;; use visual-fill-mode and consider using visual-fill-column to control the wrap width according to fill-column.
;; https://melpa.org/#/visual-fill-column https://codeberg.org/joostkremers/visual-fill-column
;;
;; turn off auto-fill and turn on visual-line.
;;
;; (auto-fill-mode)
;; (visual-line-mode)
;; (visual-line-fill-column-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
;; spell all code comments
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; RSP: use c++-mode instead of c-mode for .h files.
(setq auto-mode-alist (cons '("\\.h\\'" . c++-mode) auto-mode-alist))

;; make some common keywords stand out.
;; found on: http://emacs-fu.blogspot.com/2008/12/highlighting-todo-fixme-and-friends.html
(defvar fixme-and-friends
  '(("\\<\\(FIXME\\|TODO\\|NYI\\|TBD\\|BUG\\|XXX\\):" 1 font-lock-warning-face t)))
(font-lock-add-keywords 'python-mode fixme-and-friends)

;; ------------------------------------------------------------
;; Ruby
(autoload 'ruby-mode "ruby-mode" "Major mode for editing ruby scripts." t)
(setq auto-mode-alist (cons '("\\.rb$" . ruby-mode) auto-mode-alist))
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode)) interpreter-mode-alist))

;; ------------------------------------------------------------
;; Makefiles
(autoload 'file-mode "makefile-mode" "Makefile mode" t)
(setq auto-mode-alist (cons '("Makefile" . makefile-mode) auto-mode-alist))

;; ------------------------------------------------------------
;; SSH / Shell
(require 'comint)

(setopt comint-completion-addsuffix (quote ("/" . " ")))

;; This RE should match on any password request. It is used by
;; comint-watch-for-password-prompt.
(setopt comint-password-prompt-regexp
        "\\(\\([Oo]ld \\|[Nn]ew \\|Kerberos \\|'s \\|login \\|CVS \\|^\\)[Pp]assword\\( (again)\\)?\\|pass ?phrase\\|Enter passphrase\\)\\( for \\(RSA key \\)?[^@ \t\n]+\\(@[^@ \t\n]+\\)?\\)?\\(, try again\\)?:\\s *\\'")

(autoload 'ssh "ssh" "Allows SSH logins to act like shell-mode" t)
;; Watch for password requests & force hidden password entry.
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;; https://www.johndcook.com/blog/2016/11/30/setting-up-emacs-shell-on-a-mac/
(defun aron/shell-mode-hook--bindings ()
    (local-set-key (kbd "<M-up>") 'comint-previous-input)
    (local-set-key (kbd "<M-down>") 'comint-next-input))
(add-hook 'shell-mode-hook #'aron/shell-mode-hook--bindings)

;; (setq remote-shell-program "/usr/local/bin/ssh")
;; (setq rlogin-program       "/usr/local/bin/slogin")

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

(add-to-list 'auto-mode-alist '("\\.jslintrc\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.jshintrc\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.eslintrc\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.json.erb\\'" . json-mode))

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(require 'js)
(define-key js-mode-map (kbd "M-.") nil)

;; Stop js2 from complaining; too many things linting JS!!
(require 'js2-mode)
(setopt js2-strict-trailing-comma-warning nil) ; trailing commas are fine.
;; complaints about expect(foo).to.be.null -- code has no side effect is annoying, but
;; disabling all strict warnings is too broad.
;; probably want something like: https://github.com/mooz/js2-mode/issues/292#issuecomment-155541237
;; (setq js2-mode-show-strict-warnings nil)

(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

(setopt js-indent-level 2)

;; https://github.com/flycheck/flycheck/issues/1087#issuecomment-267587217
(eval-after-load 'js2-mode
  '(add-hook 'js2-mode-hook #'add-node-modules-path))
(eval-after-load `js2-mode
  `(add-hook 'js2-mode-hook
             (lambda ()
               (add-hook 'after-save-hook #'aron/eslint-fix-file-and-revert nil t))))

(eval-after-load `vue-mode
  `(add-hook `vue-mode-hook #'add-node-modules-path))
(eval-after-load `vue-mode
  `(add-hook 'vue-mode-hook
             (lambda ()
               (add-hook 'after-save-hook #'aron/eslint-fix-file-and-revert nil t))))
(use-package mmm-mode
  :custom
  ;; suppress the region background color, per https://github.com/AdamNiederer/vue-mode
  ;; may want to scope this just to vue-mode.
  (mmm-submode-decoration-level 0)
  :config
  ;; fix bad indents in vue JS blocks
  ;; https://github.com/AdamNiederer/vue-mode/issues/74
  ;; https://github.com/AdamNiederer/vue-mode/issues/100
  (setq mmm-js-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
  (setq mmm-typescript-mode-enter-hook (lambda () (setq syntax-ppss-table nil))))

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :custom
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (flycheck-emacs-lisp-load-path load-path)
  (flycheck-lintr-linters "NULL") ;; Use the .lintr configuration rather than the emacs configured default.
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  ;; Disable flycheck on indirect buffers (e.g., quarto-mode+polymode)
  (defun flycheck-buffer-not-indirect-p (&rest _)
    "Ensure that the current buffer is not indirect."
    (null (buffer-base-buffer)))
  (advice-add 'flycheck-may-check-automatically
              :before-while #'flycheck-buffer-not-indirect-p))

;; web-mode (better HTML+JS)
;; http://web-mode.org
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

;; might need https://github.com/editorconfig/editorconfig-emacs#customize
;; to get web-mode to play nicely with editorconfig.
;(setq web-mode-markup-indent-offset 2)
;(setq web-mode-attr-indent-offset 4)
;(setq web-mode-code-indent-offset 2)
;(setq web-mode-script-padding 2)

;; (custom-set-variables
;;  '(nxml-child-indent 4)
;;  '(js-indent-level 2) ; used by js-mode, json-mode
;;  '(js2-basic-offset 2) ; used by js2-mode
;; )


;; Markdown / RMarkdown

;; markdown-mode us used automatically but we want gfm-mode (a derived mode).
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
;; markdown-mode doesn't know about Rmd/Rmd.tmpl
(add-to-list 'auto-mode-alist '("\\.Rmd$" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd.tmpl$" . gfm-mode))

;; Quarto begin
;; markdown-mode doesn't know about qmd
;; (add-to-list 'auto-mode-alist '("\\.qmd$" . gfm-mode))

;;(require 'quarto-mode)
(require 'poly-markdown)
(add-to-list 'auto-mode-alist '("\\.qmd" . poly-markdown-mode))

;; Quarto end

;; SQL
(use-package sql-indent
  :ensure t
  :hook (sql-mode . sqlind-minor-mode))


(put 'upcase-region 'disabled nil)

;; https://github.com/zk-phi/indent-guide
(require 'indent-guide)
(set-face-foreground 'indent-guide-face "gray")
(indent-guide-global-mode)

;; http://www.lunaryorn.com/2014/09/13/boosting-which-func-mode.html
(which-function-mode)

;; https://github.com/TeMPOraL/nyan-mode
;;(nyan-mode)

;; R
;; NOTE: ess-r defines project-root, which causes all sorts of complications
;; with lsp-mode.
;; (require 'ess-r-mode)
;; leave underscore alone!
;; (ess-toggle-underscore nil)
;; (add-hook 'ess-r-mode-hook (lambda()
;;                            (ess-set-style 'RStudio)
;;                            (setq ess-align-arguments-in-calls nil)
;;                            ))

(require 'ess-r-mode)

(defun aron/ess-r-settings ()
  (ess-set-style 'RStudio)
  (setq ess-indent-offset 2)
  (setq tab-width 2)
  (setq ess-use-flymake nil) ;; disable Flymake in favor of flycheck.
)
(add-hook 'ess-r-mode-hook #'aron/ess-r-settings)
(add-hook 'ess-r-mode-hook 'eglot-ensure)
(if (file-executable-p (expand-file-name "~/bin/air"))
    (add-to-list 'eglot-server-programs
                 '((R-mode ess-r-mode) . ("~/bin/air" "language-server")))
  (warn "R language server not found: ~/bin/air"))
(defun aron/eglot-before-save-r ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
)
(add-hook 'ess-r-mode-hook #'aron/eglot-before-save-r)

;; Groovy / Jenkinsfile
(setq auto-mode-alist (cons '("Jenkinsfile" . groovy-mode) auto-mode-alist))

;; Tree-sitter grammar sources
(setq treesit-language-source-alist
      '((go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (just "https://github.com/IndianBoy42/tree-sitter-just")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (templ "https://github.com/vrischmann/tree-sitter-templ")
        (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")))

(use-package yaml-ts-mode
  :mode ("\\.yml\\'" "\\.yaml\\'" "\\.yaml\\.gotmpl\\'")
  :init
  (aron/ensure-treesit-grammar 'yaml))

(use-package just-ts-mode
  :ensure t
  :mode ("justfile\\'" "\\.just\\'")
  :init
  (aron/ensure-treesit-grammar 'just))

(use-package templ-ts-mode
  :ensure t
  :after go-ts-mode
  :mode "\\.templ\\'"
  :init
  (aron/ensure-treesit-grammar 'javascript)
  (aron/ensure-treesit-grammar 'templ))

;; gcfg isn't quite gitconfig, but it's close.
;; https://code.google.com/p/gcfg/
;; (add-to-list 'auto-mode-alist '("\\.gcfg$" . gitconfig-mode))
(add-to-list 'auto-mode-alist '("\\.gcfg$" . gcfg-mode))

(use-package python
  :mode ("\\.py\\'" . python-ts-mode)
  :init
  (aron/ensure-treesit-grammar 'python)
  :hook (python-ts-mode . eglot-ensure)
  :custom
  (python-fill-docstring-style 'django))

(if (file-executable-p (expand-file-name "~/python/env/bin/pylsp"))
    (add-to-list 'eglot-server-programs
                 '((python-mode python-ts-mode) . ("~/python/env/bin/pylsp")))
  (warn "Python language server not found: ~/python/env/bin/pylsp"))

;; Go
;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
;; https://github.com/joaotavora/eglot/issues/574
(require 'project)
(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)

;; note: https://github.com/weijiangan/flycheck-golangci-lint/issues/24
;; keep correct version of golangci-lint in PATH.
(use-package flycheck-golangci-lint
  :ensure t
  :init
  ;; hack to avoid version detection problems related to golangci-lint not being discovered because of PATH shenanigans.
  (setq flycheck-golangci-lint--version `(2 6 2))
  :hook (go-ts-mode . flycheck-golangci-lint-setup))

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

;; cmake
(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)

;; http://pragmaticemacs.com/emacs/get-pop-up-help-for-keybindings-with-which-key/
;; more help for keybindings
(which-key-mode)

;; supposedly this is how folks configure one set of styles across editors.
(require 'editorconfig)
(editorconfig-mode 1)

;; (setopt safe-local-variable-values
;;         '(
;;           (js2-basic-offset . 2)
;;           )
;;         )

;; super awesome window movement. on the mac: command-arrow.
(windmove-default-keybindings 'super)

;; magit / magithub
;;(require 'magithub)
;;(magithub-feature-autoinject t)

;; .dotfiles/.gitignore_global is not named .gitignore because it is not an
;; ignore for that repo.
(add-to-list 'auto-mode-alist '("\\.gitignore.*" . gitignore-mode))

(winner-mode 1)

;; show lines with changes in the LHS.
(global-git-gutter-mode +1)

;; https://github.com/jacktasia/dumb-jump uses:
;; git grep
;; https://github.com/ggreer/the_silver_searcher
;; https://github.com/BurntSushi/ripgrep
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;; Automatically executable scripts
;; https://emacsredux.com/blog/2021/09/29/make-script-files-executable-automatically/
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; get compilation buffers to support color output (because no one looks at TERM)
;; https://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; https://github.com/purcell/whole-line-or-region
(require 'whole-line-or-region)
(whole-line-or-region-global-mode)

(eval-after-load `hcl-mode
  `(add-hook 'hcl-mode-hook
             (lambda ()
               (add-hook 'after-save-hook #'aron/hcl-fix-file-and-revert nil t))))

(provide 'aron-init)
;;; aron-init.el ends here
