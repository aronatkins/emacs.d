;;; package ---  get it started.

;;; Commentary:
;;; You know.

;;; Code:

;; Turn off interface stuff early to avoid flicker
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)

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
(custom-set-faces
 '(font-lock-comment-face ((t (:foreground "cornflower blue")))))
(load-theme 'deeper-blue t)

(require 'aron-init)

(provide 'init)
