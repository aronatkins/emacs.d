;;; package ---  get it started.

;;; Commentary:
;;; You know.

;;; Code:

;; Turn off interface stuff early to avoid flicker
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;; fix a macos emacs maverics issue where our default-directory
;; is "/" when launched from the finder.
;; http://lists.gnu.org/archive/html/emacs-devel/2013-10/msg00498.html
;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2013-10/msg00497.html
(if (equal default-directory "/")
    (cd (getenv "HOME")))

;; http://www.emacswiki.org/emacs/ELPA
;; package.el.
;; (setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")))
;; http://emacsworld.blogspot.com/2014/10/update-your-package-search-url-as.html
(setq package-archives '(("melpa" . "http://melpa.org/packages/")))
;; found that marmalade had an old version of groovy-mode. not sure if one
;; repo is better than the others?

;; Expliticly initialize package(ELPA) so we can require its modules in
;; aron-init. This changes the default initialization order of emacs.
;; http://www.emacswiki.org/emacs/ELPA#toc6

;; (add-hook 'after-init-hook (lambda () (load "aron-init")))
;; (add-hook 'after-init-hook (lambda () (require 'aron-init)))
(setq package-enable-at-startup nil)
(package-initialize)

(require 'aron-init)

(provide 'init)
;;; init.el ends here
