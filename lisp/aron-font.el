;;; package ---  Aron's font stuff.

;;; Commentary:
;;; You know.

;;; Code:
(require 'font-lock)

;; Are we running XEmacs or Emacs?
(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

;; Turn on font-lock mode for Emacs
(cond ((not running-xemacs)
       (global-font-lock-mode t)
))

;; disable all font-lock.
;; (global-font-lock-mode nil)

;; font enabling
(setq font-lock-maximum-decoration t)
;; size of font-lock buffer (default is 256000)
;; (setq font-lock-maximum-size 1024000)
(setq font-lock-maximum-size 512000)

(remove-hook 'font-lock-mode-hook 'turn-on-fast-lock)

(custom-set-faces
 '(font-lock-comment-face ((((class color) (background dark)) (:foreground "CornflowerBlue")))))

;; (add-hook 'python-mode-hook 'turn-on-font-lock)

(provide 'aron-font)
;;; aron-font.el ends here
