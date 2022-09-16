;;; package ---  What Aron likes.

;;; Commentary:
;;; You know.

;;; Code:

;; Lots of good config examples:
;; http://www.djcbsoftware.nl/dot-emacs.html

(require 'use-package)
(require 'gcfg-mode)
(require 'aron-func)
(require 'aron-gdb-hooks)
(require 'aron-grep)
(require 'aron-keys)
(require 'aron-compile)

;; Controls for the emacs status bar.
;; (display-time)                      ; Show the time.
(line-number-mode t)                ; Show line-number.
(column-number-mode t)              ; Show column (character position)

(custom-set-variables
 '(inhibit-startup-message t)    ; Disable the emacs startup message.
 '(initial-scratch-message nil)  ; Disable information about *scratch*

 ;; trailing whitespace is nice, but it also highlights lines with
 ;; only-whitespace (ie. empty lines in a code block that happen to be
 ;; indented).
 ;; '(show-trailing-whitespace t)

'(font-lock-maximum-decoration t)

 ;; emacs (pre-23.1) used to use completion-ignore-case for both
 ;; find-file completion and buffer-switching completion.
 '(completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(read-buffer-completion-ignore-case t)

 ;; Make fill mode accept ". " as a sentence end.
 '(sentence-end-double-space nil)

 ;; reasonable max width for filled regions.
 '(fill-column               78)

 ;; show unfinished keystrokes early.
 '(echo-keystrokes 0.1)

 ;; Visual feedback on selections
 ;; (setq-default transient-mark-mode t)
 '(transient-mark-mode t)

 ;; Always end a file with a newline
 '(require-final-newline t)

 ;; Stop at the end of the file, not just add lines
 '(next-line-add-newlines nil)

 ;; Do not insert tabs.
 ;;(setq-default indent-tabs-mode nil)
 '(indent-tabs-mode nil)

 '(query-replace-highlight t)        ; highlight during query
 '(search-highlight t)               ; incremental search highlights
 '(split-width-threshold nil) ; stop L/R window splitting

 '(flycheck-disabled-checkers (quote (emacs-lisp-checkdoc)))
 '(flycheck-emacs-lisp-load-path load-path)

 ;; BUG: This is platform-specific (Mac). Uses aspell installed with homebrew.
 ;; brew install aspell
;; '(ispell-program-name "/usr/local/bin/aspell")

 '(ess-use-flymake nil) ;; disable Flymake in favor of flycheck.
 '(ess-r-flymake-linters "NULL")
 '(flycheck-lintr-linters "NULL") ;; Use the .lintr configuration rather than the emacs configured default.

 '(windmove-wrap-around t)

  ; ask me before death. Command-q is an accident!
 '(confirm-kill-emacs #'y-or-n-p)

 ; quickly help an old man.
 '(which-key-idle-delay 0.5)

 ;; control-L behavior. http://irreal.org/blog/?p=6436
 ;; also. try out C-M-l !!!
 ;; '(recenter-positions '(top middle bottom))

 ;; stop gfm (markdown) mode from having electric backticks.
 '(markdown-gfm-use-electric-backquote nil)

 ;; tuning for LSP (https://emacs-lsp.github.io/lsp-mode/page/performance/#tuning)
 '(gc-cons-threshold 100000000)
 '(read-process-output-max (* 1024 1024)) ;; 1mb

 ;; https://www.emacswiki.org/emacs/AlarmBell
 '(ring-bell-function 'ignore)
 )

;; make cursor the width of the character it is under
;; i.e. full width of a TAB
;; (setq x-stretch-cursor t)
;; doesn't interact well with indent-guide.

;; Files to auto-revert when reloaded.
; (setq revert-without-query '(".*\.[ch]"))

;; (setq enable-recursive-minibuffers t)
;; (setq version-control nil)           ; numbered backups for files which have them

;;(require 'icomplete)        ; active minibuffer completion
;;(icomplete-mode)

;; https://www.masteringemacs.org/article/introduction-to-ido-mode
;; display any item that contains the typed chars .. quite a shift
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-create-new-buffer 'always)
(ido-mode t)

(defalias 'list-buffers 'ibuffer)       ; A richer list-buffers experience.

; uniquify: buffer names are uniquified with parts of the file path.
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-after-kill-buffer-p t)

;;(setq auto-compression-mode t)          ;; auto-handle .gz and .Z files
(auto-compression-mode t)

(put 'narrow-to-region 'disabled nil)

;; (put 'downcase-region 'disabled nil) ;; What's this do?

; Setting this variable will cause the compile buffer to always stay at the end.
(setq compilation-scroll-output t)
;; compilation-spawned shells are "interactive", meaning we get .bashrc
;; https://stackoverflow.com/a/17595062
(defadvice compile (around use-bashrc activate)
  "Load .bashrc in any calls to bash (e.g. so we can use aliases)"
  (let ((shell-command-switch "-ic"))
    ad-do-it))

; symmetric scroll up/down. http://irreal.org/blog/?p=3963
(setq scroll-preserve-screen-position 'always)

;; http://pragmaticemacs.com/emacs/volatile-highlights/
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; fill all text. spell all text.
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'flyspell-mode)
;; spell all code comments
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; ------------------------------------------------------------
;; Force M-x ftp to use 'sftp' instead of 'ftp' for connections.
(require 'net-utils)
(setq ftp-program "sftp")

;; ------------------------------------------------------------
;; Java
(require 'cc-mode)
(defun bc-java-mode-hook ()
  (setq c-basic-offset 4))

;;  (interactive)
;;  (setq c-basic-offset 2)
;; (setq c-basic-offset 4)
;;  (c-set-style "java"))

(require 'gud)
(add-hook 'gdb-mode-hook 'aron/gdb-mode-hook)
(autoload 'aron/gdb-mode-hook "aron-gdb-hooks")

;; RSP: use c++-mode instead of c-mode for .h files.
(setq auto-mode-alist (cons '("\\.h\\'" . c++-mode) auto-mode-alist))

(add-hook 'java-mode-hook 'bc-java-mode-hook)

(require 'java-mode-indent-annotations)

;; c-subword-mode treats changes of case to be word boundaries. the
;; java convention is to use camelcase, so this is usually good.
(add-hook 'java-mode-hook 'subword-mode)
;; stolen from http://www.emacswiki.org/emacs/IndentingC
(defun aron/java-mode-hook ()
  (c-set-offset 'case-label '+))       ; indent case labels by c-indent-level, too
(add-hook 'java-mode-hook 'aron/java-mode-hook)

;; avoid problem with java property file quote formatting
;; http://emacsblog.org/2007/03/01/quick-tip-highlighting-java-properties-files/
(add-hook 'conf-javaprop-mode-hook
          '(lambda () (conf-quote-normal nil)))

;; the annotations setup causes @ann to not trigger indentation on
;; newline. a declaration would line up the "right way."
;;
;;   @CrappyAnnotation
;;   public void doCrap() { }
;;
;; without this hook, the method gets indented.
(add-hook 'java-mode-hook 'java-mode-indent-annotations-setup)

;; make some common keywords stand out.
;; found on: http://emacs-fu.blogspot.com/2008/12/highlighting-todo-fixme-and-friends.html
(defvar fixme-and-friends
  '(("\\<\\(FIXME\\|TODO\\|NYI\\|TBD\\|BUG\\|XXX\\):" 1 font-lock-warning-face t)))
(font-lock-add-keywords 'java-mode fixme-and-friends)
(font-lock-add-keywords 'python-mode fixme-and-friends)

;; ------------------------------------------------------------
;; Python

;; configure docstring formatting.
(custom-set-variables
 '(python-fill-docstring-style 'django)    ; Disable the emacs startup message.
)


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

(setq comint-completion-addsuffix (quote ("/" . " ")))

;; This RE should match on any password request. It is used by
;; comint-watch-for-password-prompt.
(setq comint-password-prompt-regexp
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


;; ------------------------------------------------------------
;; Perforce
;;
;; The P4 library takes quite a while to load.. is it because we
;; haven't set up the environment variables yet?
;;
;; Reference:
;; http://p4el.sourceforge.net/p4.el.html
(if (or (getenv "P4CONFIG") (getenv "P4CLIENT"))
    (load-library "p4"))

(put 'narrow-to-region 'disabled nil)

;; ------------------------------------------------------------
;; PMD integration.
;; (defvar pmd-lisp-path (concat contrib-lisp-path "/pmd")
;;   "*Path to my version of pmd.el load libraries.")
;; (add-to-list 'load-path pmd-lisp-path)
;;
;; (setq opt-home (concat (getenv "HOME") "/opt"))
;; (setq pmd-java-home (concat opt-home "/jdk/bin/java"))
;; (setq pmd-home (concat opt-home "/pmd"))
;; (setq pmd-ruleset-list (list "basic" "braces" "codesize" "design" "naming" "imports" "unusedcode"))
;;
;; (require 'pmd)

;; C-x f //ssh:aron@host.com:/path/to/something
;; with ssh as default, can just:
;; C-x f /aron@host.com:/path/to/something
;;
;; a good article with a "sudo-edit"
;; http://nflath.com/2009/08/tramp/
;;
;; with ubuntu 11.10, tramp REALLY affects emacs startup time. it also
;; appears that tramp is auto-loaded when you type in a filename that
;; looks like a tramp file. like /shazbot:silly.txt
;;
;;(require 'tramp)
;;(setq tramp-default-method "ssh")

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

(add-to-list 'auto-mode-alist '("\\.jslintrc\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.jshintrc\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.eslintrc\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.json.erb\\'" . json-mode))

;; http://www.emacswiki.org/emacs/NodeJs
;; http://js-comint-el.sourceforge.net
;; also consider: https://github.com/abicky/nodejs-repl.el
;;(require 'js-comint)

;; may need to add --interactive
(setq inferior-js-program-command
      (let ((personal-node (substitute-in-file-name "$HOME/opt/node/bin/node")))
        (if (file-exists-p personal-node)
            personal-node
          "node")))

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(require 'js)
(define-key js-mode-map (kbd "M-.") nil)

;; Stop js2 from complaining; too many things linting JS!!
(setq js2-strict-trailing-comma-warning nil) ; trailing commas are fine.
;; complaints about expect(foo).to.be.null -- code has no side effect is annoying, but
;; disabling all strict warnings is too broad.
;; probably want something like: https://github.com/mooz/js2-mode/issues/292#issuecomment-155541237
;; (setq js2-mode-show-strict-warnings nil)

(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

(setq js-indent-level 2)

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
;; fix bad indents in vue JS blocks
;; https://github.com/AdamNiederer/vue-mode/issues/74
;; https://github.com/AdamNiederer/vue-mode/issues/100
(setq mmm-js-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
(setq mmm-typescript-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
;; suppress the region background color, per https://github.com/AdamNiederer/vue-mode
;; may want to scope this just to vue-mode.
(setq mmm-submode-decoration-level 0)
  
(add-hook 'after-init-hook #'global-flycheck-mode)

(eval-after-load 'flycheck
  '(flycheck-add-mode 'javascript-eslint 'web-mode))

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
(require 'quarto-mode)

;; disable flycheck on indirect buffers, such as those created by
;; quarto-mode+polymode.
(defun flycheck-buffer-not-indirect-p (&rest _)
  "Ensure that the current buffer is not indirect."
  (null (buffer-base-buffer)))

(advice-add 'flycheck-may-check-automatically
            :before-while #'flycheck-buffer-not-indirect-p)

;; Quarto end

;; YAML
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml.gotmpl$" . yaml-mode))

;; SQL
(eval-after-load "sql"
  '(load-library "sql-indent"))


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
(add-hook 'ess-mode-hook (lambda()
                           (ess-set-style 'RStudio)
                           (setq ess-align-arguments-in-calls nil)
                           ))

;; Groovy / Jenkinsfile
(setq auto-mode-alist (cons '("Jenkinsfile" . groovy-mode) auto-mode-alist))

;; Go

;; examples
;; http://joelmccracken.github.io/entries/project-local-variables-in-projectile-with-dirlocals/
;; https://seandavi.github.io/post/2018-12-08-directory-local-variables-for-custom-emacs-projects/

;; This isn't right always, but is good for connect.
;; TODO: set in a context-aware way.
;;
;; try adding to connect/.dir-locals.el; inspired by
;; https://github.com/nelsam/prelude/blob/nelsam/gopath-dir-locals.el
;; ((go-mode . (
;;              (eval . (setq project-gopath
;;                            (expand-file-name
;;                             (locate-dominating-file buffer-file-name ".dir-locals.el"))))
;;              (eval . (setenv "GOPATH" project-gopath))
;;              )))
;; tried this first; didn't work
;; ((nil . ((eval . (progn
;;                   (setenv "GOPATH" (expand-file-name "."))
;;               )))))
;; also look at using (projectile-project-root)
;;
;; The old way was to have this globally:
;;
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Directory-Local-Variables.html
;; (setenv "GOPATH" (expand-file-name "dev/rstudio/connect" (getenv "HOME")))

;; (setq safe-local-variable-values
;;       (quote
;;        (
;;         (eval setq project-gopath
;;               (expand-file-name
;;                (locate-dominating-file buffer-file-name ".dir-locals.el")))
;;         (eval setenv "GOPATH" project-gopath)
;;         (eval setenv "GOPRIVATE" "github.com/rstudio,connect,timestamper,envmanager")
;;         ; (eval setenv "GOFLAGS" "-mod=vendor")
;;         (eval setenv "GOCACHE" (concat project-gopath "cache/go"))
;;         )))

(setq safe-local-variable-values
      (quote
       (
        (eval . (setq connect-root (expand-file-name (locate-dominating-file default-directory ".dir-locals.el"))))
         
        ;; lsp-mode wants to use the Connect root as its workspace root by default.
        (eval . (lsp-workspace-folders-add (concat connect-root "src/connect")))
        (eval . (lsp-workspace-folders-add (concat connect-root "src/generate")))
        (eval . (lsp-workspace-folders-add (concat connect-root "src/timestamper")))
        (eval . (lsp-workspace-folders-add (concat connect-root "src/linkwalk")))
        (eval . (lsp-workspace-folders-add (concat connect-root "src/envmanager")))
        (eval . (lsp-workspace-folders-add (concat connect-root "src/rsc-quarto")))
        (eval . (lsp-workspace-folders-add (concat connect-root "src/rsc-session")))
        ;; GOPATH because lsp-mode cannot cope with our repo
        ;; https://github.com/golang/go/issues/36899
        ;;(eval setenv "GOPATH" project-gopath)
        ;; GOPRIVATE so lsp-go does not offer links for private packages
        ;; https://github.com/golang/go/issues/36998
        ;; (eval . (setenv "GOPATH" connect-root))
        (eval . (setenv "GOPRIVATE" "github.com/rstudio,connect,timestamper,linkwalk,envmanager,rsc-quarto,rsc-session"))
        (eval . (setenv "GOCACHE" (concat connect-root "cache/go")))
        (eval . (setenv "GOMODCACHE" (concat connect-root "pkg/mod")))
        ;; (eval . (setenv "GOFLAGS" "-mod=vendor"))
        )))

;; (projectile-mode +1)
;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; gcfg isn't quite gitconfig, but it's close.
;; https://code.google.com/p/gcfg/
;; (add-to-list 'auto-mode-alist '("\\.gcfg$" . gitconfig-mode))
(add-to-list 'auto-mode-alist '("\\.gcfg$" . gcfg-mode))

;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md#emacs
;; https://github.com/golang/go/issues/36899
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred)
  :config
  (lsp-register-custom-settings
   `(
     ("gopls.local" "connect" t)
     ;; ("gopls.experimentalWorkspaceModule" t t)
     ))
  ;; :custom
  ;; (lsp-gopls-use-placeholders t)
  ;; :config
  ;; (lsp-register-custom-settings
  ;;  '(("gopls.completeUnimported" t t)
  ;;    ("gopls.staticcheck" t t)))
  ;; :config
  ;; (lsp-register-custom-settings
  ;;  '(("gopls.linkTarget" "" t)))
  )

;; https://emacs.blog/2022/02/20/golang-ide-setup-in-emacs/
;; https://github.com/golang/go/issues/50955

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(use-package go-mode
  :ensure t
  :bind (("C-c i" . go-goto-imports)
         ("C-c C-c" . aron/go-compile)
         ;; ("C-c C-s" . aron/go-start) ;; BROKEN
         ("C-c C-t" . aron/go-test)
         )
  )

;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

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
  :hook (go-mode . yas-minor-mode))

;; gopls customization example
;; (lsp-register-custom-settings
;;  '(("gopls.completeUnimported" t t)
;;    ("gopls.staticcheck" t t)))

;; (defun aron/go-mode-hook--gofmt ()
;;   (add-hook 'before-save-hook 'gofmt-before-save))
;; (add-hook 'go-mode-hook #'aron/go-mode-hook--gofmt)
;; (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)

;;(require 'flycheck-gometalinter)
;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-gometalinter-setup))

;; skips 'vendor' directories and sets GO15VENDOREXPERIMENT=1
;;(setq flycheck-gometalinter-vendor t)
;; disable linters
;; (setq flycheck-gometalinter-disable-linters '("gotype" "gocyclo"))
;; Only enable selected linters
;; (setq flycheck-gometalinter-disable-all t)

;; gotype requires all dependent packages have been built. which isn't great.
;; (setq flycheck-gometalinter-enable-linters '("vet" "vetshadow" "golint" "goconst" "ineffassign"))
;; (setq flycheck-gometalinter-enable-linters '("vet" "vetshadow"))


;; Set different deadline (default: 5s)
;(setq flycheck-gometalinter-deadline "10s")

;; if we have golint, use it.
;; (let (
;;       (golint-location (concat (getenv "HOME")  "/go/src/github.com/golang/lint/misc/emacs"))
;;       )
;;   (if (file-exists-p golint-location)
;;       (progn
;;         (add-to-list 'load-path golint-location)
;;         (require 'golint)
;;         )))

;; (require 'lsp-mode)
;; (add-hook 'go-mode-hook 'lsp-deferred)

;; cmake
(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)

;; http://pragmaticemacs.com/emacs/get-pop-up-help-for-keybindings-with-which-key/
;; more help for keybindings
(which-key-mode)

;; C-c /-XXX
(require 'google-this)
(google-this-mode 1)

;; supposedly this is how folks configure one set of styles across editors.
(require 'editorconfig)
(editorconfig-mode 1)

;; super awesome window movement. on the mac: command-arrow.
(windmove-default-keybindings 'super)

;; magit / magithub
;;(require 'magithub)
;;(magithub-feature-autoinject t)

(winner-mode 1)

;; show lines with changes in the LHS.
(global-git-gutter-mode +1)

;; https://github.com/jacktasia/dumb-jump uses:
;; git grep
;; https://github.com/ggreer/the_silver_searcher
;; https://github.com/BurntSushi/ripgrep
(dumb-jump-mode)

;; Automatically executable scripts
;; https://emacsredux.com/blog/2021/09/29/make-script-files-executable-automatically/
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; get compilation buffers to support color output (because no one looks at TERM)
;; https://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

; todotxt-mode not available from melpa-stable.
;;(require 'todotxt-mode)
;;(add-to-list 'auto-mode-alist '("\\todo.txt\\'" . todotxt-mode))
;;(setq todotxt-default-file (expand-file-name "~/todo.txt"))

;; https://github.com/purcell/whole-line-or-region
(require 'whole-line-or-region)
(whole-line-or-region-global-mode)

(eval-after-load `hcl-mode
  `(add-hook 'hcl-mode-hook
             (lambda ()
               (add-hook 'after-save-hook #'aron/packer-fix-file-and-revert nil t))))

(provide 'aron-init)
;;; aron-init.el ends here
