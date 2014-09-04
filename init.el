;;; package ---  get it started.

;;; Commentary:
;;; You know.

;;; Code:

;; Turn off interface stuff early to avoid flicker
(menu-bar-mode -1)
(tool-bar-mode -1)
;;(scroll-bar-mode -1)
(setq inhibit-startup-message t)

(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;; http://www.emacswiki.org/emacs/ELPA
;; package.el.
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")))
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
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(completion-ignore-case t t)
;;  '(custom-safe-themes (quote ("146d24de1bb61ddfa64062c29b5ff57065552a7c4019bee5d869e938782dfc2a" default)))
;;  '(fill-column 78)
;;  '(indent-tabs-mode nil)
;;  '(inhibit-startup-screen t)
;;  '(initial-scratch-message nil)
;;  '(js-indent-level 2)
;;  '(next-line-add-newlines nil)
;;  '(nxml-child-indent 4)
;;  '(python-fill-docstring-style (quote django))
;;  '(query-replace-highlight t)
;;  '(read-buffer-completion-ignore-case t)
;;  '(read-file-name-completion-ignore-case t)
;;  '(require-final-newline t)
;;  '(search-highlight t)
;;  '(sentence-end-double-space nil)
;;  '(text-mode-hook (quote turn-on-auto-fill))
;;  '(transient-mark-mode t))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(font-lock-comment-face ((((class color) (background dark)) (:foreground "CornflowerBlue")))))
