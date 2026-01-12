;;; package ---  get it started.

;;; Commentary:
;;; You know.

;;; Code:

;; Turn off interface stuff early to avoid flicker
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)

;; Mode line indicators
(line-number-mode t)
(column-number-mode t)

;; the title bar shares the background color
;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; or nil to switch to dark title text
;; (add-to-list 'default-frame-alist '(ns-appearance . dark))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;; fix a macos emacs maverics issue where our default-directory
;; is "/" when launched from the finder.
;; http://lists.gnu.org/archive/html/emacs-devel/2013-10/msg00498.html
;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2013-10/msg00497.html
(if (equal default-directory "/")
    (cd (getenv "HOME")))

; From: https://melpa.org/#/getting-started
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
; (package-initialize)

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

;; a theme to this story
;; https://emacsthemes.com

;; deeper-blue has a default mid-grey comment face.
;; (custom-set-faces
;;  '(font-lock-comment-face ((t (:foreground "cornflower blue")))))
;; (load-theme 'deeper-blue t)
;; (disable-theme 'whiteboard)

;; disable scaling of text in things like Markdown files; must happen prior to
;; theme-load.
(require 'leuven-theme)
(setq leuven-scale-outline-headlines nil)
(setq leuven-scale-org-agenda-structure nil)
(setq leuven-scale-volatile-highlight nil)
(load-theme 'leuven t)

(require 'aron-init)

(provide 'init)
