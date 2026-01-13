;;; init.el --- Emacs initialization  -*- lexical-binding: t; -*-

;;; Commentary:
;; Main entry point for Emacs configuration.

;;; Code:


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

;; Add MELPA to package archives
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

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

;; disable scaling of text in things like Markdown files
(use-package leuven-theme
  :ensure t
  :custom
  (leuven-scale-outline-headlines nil)
  (leuven-scale-org-agenda-structure nil)
  (leuven-scale-volatile-highlight nil)
  :config
  (load-theme 'leuven t))

(require 'aron-init)

(provide 'init)
;;; init.el ends here
