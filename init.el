;; Turn off interface stuff early to avoid flicker
(menu-bar-mode -1)
(tool-bar-mode -1)
;;(scroll-bar-mode -1)
(setq inhibit-startup-message t)

(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path (expand-file-name "contrib" user-emacs-directory))

(require 'aron-init)
