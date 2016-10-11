;; run: emacs --script packages.el
;;
;; adapted from:
;; http://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name
(require 'package)

;; From: https://github.com/milkypostman/melpa#usage
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(package-refresh-contents)

(package-initialize)

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (not (package-installed-p package))
         (package-install package)))
   packages))

(ensure-package-installed
 'bug-hunter
 'cmake-mode
 'cmake-font-lock
 'crux
 'define-word
 'ess
 'exec-path-from-shell
 'expand-region
 'flycheck
 'flycheck-gometalinter
 'git-timemachine
 'go-mode
 'graphviz-dot-mode
 'groovy-mode
 'hungry-delete  ;; not loaded by default
 'indent-guide
 ;;'js-comint
 'js2-mode
 'json-mode
 'markdown-mode
 'magit
 'gitconfig-mode
 'gitignore-mode
 'google-this
 'nyan-mode
 'projectile
 'reveal-in-osx-finder
 'sql-indent
 'vagrant
 'vagrant-tramp
 'volatile-highlights
 'web-mode                              ; possibly better HTML+JS
 'which-key
 'yaml-mode
 'ztree
 )

;; activate installed packages
