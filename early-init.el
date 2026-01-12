;;; early-init.el --- Early initialization  -*- lexical-binding: t; -*-

;;; Commentary:
;; Loaded before the GUI is initialized.

;;; Code:

;; Disable UI elements before they're rendered (avoids flicker)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq inhibit-startup-message t)

;; Prevent package.el from loading packages before init.el runs
(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
