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
 'auto-complete
 'bug-hunter
 'cmake-mode
 'cmake-font-lock
 'crux
 'define-word
 'dockerfile-mode
 'dumb-jump
 'editorconfig
 'ess
 'exec-path-from-shell
 'expand-region
 'flycheck
 'flycheck-gometalinter
 'git-timemachine
 'git-gutter
 'go-dlv
 'go-guru
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
 ;; 'magithub
 'gitconfig-mode
 'gitignore-mode
 'google-this
 'nyan-mode
 'prettier-js
 'projectile
 'reveal-in-osx-finder
 'sql-indent
 'vagrant
 'vagrant-tramp
 'volatile-highlights
 'vue-mode
 'web-mode                              ; possibly better HTML+JS
 'which-key
 'xref-js2
 'yaml-mode
 'ztree
 )

;; activate installed packages
