;; run: emacs --script packages.el
;;
;; adapted from:
;; http://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name

; From: https://melpa.org/#/getting-started
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
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
 'ess
 'exec-path-from-shell
 'expand-region
 'flycheck
 'flycheck-golangci-lint
 'git-gutter
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
 'quarto-mode
 'reveal-in-osx-finder
 'rust-mode
 'sql-indent
 'templ-ts-mode
 'vagrant
 'visual-fill-column
 'volatile-highlights
 'vue-mode
 'web-mode                              ; possibly better HTML+JS
 'whole-line-or-region
 'writegood-mode
 'xref-js2
 'yasnippet
 'ztree
 )

;; activate installed packages
