;; run: emacs --script packages.el
;;
;; adapted from:
;; http://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name

; From: https://stable.melpa.org/#/getting-started
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; make sure to have downloaded archive description.
(package-refresh-contents)

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (not (package-installed-p package))
         (package-install package)))
   packages))

(ensure-package-installed
 'add-node-modules-path
 'bats-mode
 'cmake-mode
 'cmake-font-lock ; not in melpa-stable
 'company
 'crux
 'deadgrep ; for ripgrep (see: https://irreal.org/blog/?p=11097, https://github.com/BurntSushi/ripgrep & https://github.com/Wilfred/deadgrep)
 'dockerfile-mode
 'dumb-jump
 'editorconfig
 'ess
 'exec-path-from-shell
 'expand-region
 'flycheck
 'flycheck-golangci-lint
 'git-gutter
 'go-mode
 ;; 'gptel
 'graphviz-dot-mode
 'groovy-mode
 'hcl-mode
 'indent-guide
 'js2-mode
 'json-mode
 ;; https://github.com/leon-barrett/just-mode.el
 ;; 'just-mode
 ;; https://github.com/leon-barrett/just-ts-mode.el
 'just-ts-mode
 ;; https://github.com/fniessen/emacs-leuven-theme
 ;; leuven is bundled with emacs, but may be outdated.
 'leuven-theme
 ;; https://github.com/psibi/justl.el
 ;; help for executing justfile targets; not using yet.
 'lua-mode
 'markdown-mode
 'magit
 'nginx-mode
 ;; 'magithub
 'git-modes
 'projectile
 'quarto-mode
 'reveal-in-osx-finder
 'rust-mode
 'sql-indent
;; 'templ-ts-mode
 'todotxt ; not in melpa-stable
 'use-package
 'vagrant
 'visual-fill-column
 'volatile-highlights
 'vue-mode
 'web-mode                              ; possibly better HTML+JS
 'which-key
 'whole-line-or-region
 'writegood-mode
 'xref-js2
 'yaml-mode
 'yasnippet
 'ztree
 )

;; activate installed packages
