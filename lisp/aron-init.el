;;; package ---  What Aron likes.

;;; Commentary:
;;; You know.

;;; Code:

;; Lots of good config examples:
;; http://www.djcbsoftware.nl/dot-emacs.html

(require 'aron-dev)
(require 'aron-font)
(require 'aron-func)
(require 'aron-gdb-hooks)
(require 'aron-grep)
(require 'aron-keys)
(require 'bc-compile)

;; Controls for the emacs status bar.
(display-time)                      ; Show the time.
(line-number-mode t)                ; Show line-number.
(column-number-mode t)              ; Show column (character position)

(custom-set-variables
 '(inhibit-startup-message t)    ; Disable the emacs startup message.
 '(initial-scratch-message nil)  ; Disable information about *scratch*

 ;; trailing whitespace is nice, but it also highlights lines with
 ;; only-whitespace (ie. empty lines in a code block that happen to be
 ;; indented).
 ;; '(show-trailing-whitespace t)

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

 '(ess-default-style (quote GNU))
 '(ess-indent-with-fancy-comments nil)
 )

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
(ido-mode 1)

(defalias 'list-buffers 'ibuffer)       ; A richer list-buffers experience.

; uniquify: buffer names are uniquified with parts of the file path.
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-after-kill-buffer-p t)

;;(setq auto-compression-mode t)          ;; auto-handle .gz and .Z files
(auto-compression-mode t)

(require 'comint)
(setq comint-completion-addsuffix (quote ("/" . " ")))
(setq completion-ignored-extensions (quote ("CVS/" ".o" "~" ".bin" ".lbin" ".fasl" ".ufsl" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".dvi" ".fmt" ".tfm" ".pdf" ".class" ".fas" ".lib" ".x86f" ".sparcf" ".lo" ".la" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs")))

(put 'narrow-to-region 'disabled nil)

;; (put 'downcase-region 'disabled nil) ;; What's this do?

; Setting this variable will cause the compile buffer to always stay at the end.
(setq compilation-scroll-output t)

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

(add-hook 'java-mode-hook 'bc-java-mode-hook)

(require 'java-mode-indent-annotations)

;; c-subword-mode treats changes of case to be word boundaries. the
;; java convention is to use camelcase, so this is usually good.
(add-hook 'java-mode-hook 'subword-mode)
;; stolen from http://www.emacswiki.org/emacs/IndentingC
(defun aron-java-mode-hook ()
  (c-set-offset 'case-label '+))       ; indent case labels by c-indent-level, too
(add-hook 'java-mode-hook 'aron-java-mode-hook)

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

;; i'm a rule breaker.
(add-hook 'java-mode-hook '(lambda() (setq c-basic-offset 2)))

(defun bc-spacing ()
  (interactive)
  (setq c-basic-offset 4))
(defun wn-spacing ()
  (interactive)
  (setq c-basic-offset 2))

;; make some common keywords stand out.
;; found on: http://emacs-fu.blogspot.com/2008/12/highlighting-todo-fixme-and-friends.html
(defvar fixme-and-friends
  '(("\\<\\(FIXME\\|TODO\\|NYI\\|TBD\\|BUG\\|XXX\\):" 1 font-lock-warning-face t)))
(font-lock-add-keywords 'java-mode fixme-and-friends)
(font-lock-add-keywords 'python-mode fixme-and-friends)

;; ------------------------------------------------------------
;; Python
;;(require 'python-mode)
;(autoload 'python-mode "python-mode" "Python mode" t)

;; some Linux distributions already configure auto-mode-alist and
;; interpreter-mode-alist for Python, but we can't be sure.
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist
      (append '(("python"    . python-mode)
		("python2.2" . python-mode)) interpreter-mode-alist))
;(autoload 'python-mode "python-mode" "Python editing mode." t)

;;(add-hook 'python-mode-hook '(lambda () (setq py-indent-offset 2)) )

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

;; This RE should match on any password request. It is used by
;; comint-watch-for-password-prompt.
(setq comint-password-prompt-regexp
      "\\(\\([Oo]ld \\|[Nn]ew \\|Kerberos \\|'s \\|login \\|CVS \\|^\\)[Pp]assword\\( (again)\\)?\\|pass ?phrase\\|Enter passphrase\\)\\( for \\(RSA key \\)?[^@ \t\n]+\\(@[^@ \t\n]+\\)?\\)?\\(, try again\\)?:\\s *\\'")

(autoload 'ssh "ssh" "Allows SSH logins to act like shell-mode" t)
;; Watch for password requests & force hidden password entry.
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

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

;;(autoload 'js3-mode "js3" nil t)
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

(add-hook 'js3-mode-hook '(lambda ()
			    (local-set-key "\C-x\C-e" 'js-send-last-sexp)
			    (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
			    (local-set-key "\C-cb" 'js-send-buffer)
			    (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
			    (local-set-key "\C-cl" 'js-load-file-and-go)
			    ))

;; requires 'tern' in path; npm install -g tern
;; (add-hook 'js3-mode-hook '(lambda () (tern-mode t)))

;; http://stackoverflow.com/questions/9390770/node-js-prompt-can-not-show-in-eshell
(setenv "NODE_NO_READLINE" "1")

(add-hook 'after-init-hook #'global-flycheck-mode)

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(custom-set-variables
 '(nxml-child-indent 4)
 '(js-indent-level 2) ; used by js-mode, json-mode
 '(js2-basic-offset 2) ; used by js2-mode
)

(eval-after-load "sql"
  '(load-library "sql-indent"))

(put 'upcase-region 'disabled nil)

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; https://github.com/zk-phi/indent-guide
(require 'indent-guide)
(set-face-foreground 'indent-guide-face "gray")
(indent-guide-global-mode)

;; http://www.lunaryorn.com/2014/09/13/boosting-which-func-mode.html
(which-function-mode)

;; https://github.com/TeMPOraL/nyan-mode
;;(nyan-mode)

;; R
(require 'ess-site)
;; leave underscore alone!
(ess-toggle-underscore nil)

;; Groovy / Jenkinsfile
(setq auto-mode-alist (cons '("Jenkinsfile" . groovy-mode) auto-mode-alist))

;; Go

;; export GOPATH=/Users/aron/go
;; export PATH="$GOPATH/bin:$PATH"
;; go get github.com/rogpeppe/godef

;; This isn't right always, but is good for connect.
;; TODO: set in a context-aware way.
(setenv "GOPATH" (concat
                  (expand-file-name "dev/rstudio/connect/_vendor" (getenv "HOME")) ":"
                  (expand-file-name "dev/rstudio/connect" (getenv "HOME"))))

;; As of Go-1.4, editor plugins are no longer part of the go distribution.
;; https://github.com/dominikh/go-mode.el
;; (require 'go-mode-autoloads)

;; gcfg isn't quite gitconfig, but it's close.
;; https://code.google.com/p/gcfg/
(add-to-list 'auto-mode-alist '("\\.gcfg$" . gitconfig-mode))
;; http://tleyden.github.io/blog/2014/05/27/configure-emacs-as-a-go-editor-from-scratch-part-2/
;; http://dominik.honnef.co/posts/2013/03/writing_go_in_emacs/
(defun aron/go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save))
(add-hook 'go-mode-hook 'aron/go-mode-hook)
;; this one doesn't work with connect because GOPATH is not set appropriately.
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c i") 'go-goto-imports)))

;;(require 'flycheck-gometalinter)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-gometalinter-setup))

;; skips 'vendor' directories and sets GO15VENDOREXPERIMENT=1
;;(setq flycheck-gometalinter-vendor t)
;; disable linters
;; (setq flycheck-gometalinter-disable-linters '("gotype" "gocyclo"))
;; Only enable selected linters
(setq flycheck-gometalinter-disable-all t)

;; gotype requires all dependent packages have been built. which isn't great.
(setq flycheck-gometalinter-enable-linters '("vet" "vetshadow"))
;; Set different deadline (default: 5s)
;(setq flycheck-gometalinter-deadline "10s")

(eval-after-load 'flycheck
  '(flycheck-add-mode 'javascript-eslint 'web-mode))

;; cmake
(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)

;; http://pragmaticemacs.com/emacs/get-pop-up-help-for-keybindings-with-which-key/
;; more help for keybindings
(which-key-mode)

;; keep *scratch* around - http://pragmaticemacs.com/emacs/a-persistent-scratch-buffer/
(persistent-scratch-setup-default)

;; C-c /-XXX
(require 'google-this)
(google-this-mode 1)

;; web-mode (better HTML+JS)
;; http://web-mode.org
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

;; might need https://github.com/editorconfig/editorconfig-emacs#customize
;; to get web-mode to play nicely with editorconfig.
(setq web-mode-markup-indent-offset 2)
(setq web-mode-attr-indent-offset 4)
(setq web-mode-code-indent-offset 2)
(setq web-mode-script-padding 2)

;; supposedly this is how folks configure one set of styles across editors.
(require 'editorconfig)
(editorconfig-mode 1)

(provide 'aron-init)
;;; aron-init.el ends here
