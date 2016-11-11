;;; package ---  get it started.

;;; Commentary:
;;; You know.

;;; Code:

;; Turn off interface stuff early to avoid flicker
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)

;; a theme to this story
;; https://emacsthemes.com
(load-theme 'deeper-blue t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;; fix a macos emacs maverics issue where our default-directory
;; is "/" when launched from the finder.
;; http://lists.gnu.org/archive/html/emacs-devel/2013-10/msg00498.html
;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2013-10/msg00497.html
(if (equal default-directory "/")
    (cd (getenv "HOME")))

(require 'package)

;; http://melpa.milkbox.net/#/getting-started
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; Expliticly initialize package(ELPA) so we can require its modules in
;; aron-init. This changes the default initialization order of emacs.
;; http://www.emacswiki.org/emacs/ELPA#toc6

;; (add-hook 'after-init-hook (lambda () (load "aron-init")))
;; (add-hook 'after-init-hook (lambda () (require 'aron-init)))
(setq package-enable-at-startup nil)
(package-initialize)

;; Fix PATH on OSX.
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(require 'aron-init)

(provide 'init)
