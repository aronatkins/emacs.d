;;; package ---  Aron's key bindings

;;; Commentary:
;;; You know.

;;; Code:

(require 'aron-func)
(require 'aron-grep)
(require 'bc-compile)

;; fix the backspace problems on many terminals
;(global-set-key "\C-x\C-h" 'help-command)
;(global-set-key "\C-h" 'delete-backward-char)
;(global-set-key "\C-?" 'delete-char)

;; use C-z as a user-meta key, since almost everything else is used!
(defvar ctl-z-map (make-sparse-keymap) "keymap for user extensions")
(defconst ctl-z-map-prefix "\C-z")

;; since we're overloading C-z, we need to add suspend back into the
;; fray. we use C-z C-z
(define-key ctl-z-map "\C-z" 'suspend-emacs)

;; (define-key ctl-z-map "c" 'cc-checkout )
;; (define-key ctl-z-map "c" 'chry-grep-c-files)
(define-key ctl-z-map "f" 'flush-lines )
(define-key ctl-z-map "g" 'goto-line)
(define-key ctl-z-map "h" 'aron-jdk-help)
(define-key ctl-z-map "k" 'copy-region-as-kill )
;; (define-key ctl-z-map "l" 'line-number-mode)
;; (define-key ctl-z-map "n" 'chry-narrow-to-function)
(define-key ctl-z-map "q" 'query-replace)
(define-key ctl-z-map "r" 'replace-string)
;; (define-key ctl-z-map "s" 'chry-grep-local-files)
;;(define-key ctl-z-map "v" 'cds-visit-tags-table )
(define-key ctl-z-map "w" 'widen )

(global-set-key ctl-z-map-prefix ctl-z-map)

;; define this because it works well with "C-x o"
(define-key ctl-x-map "p" 'backward-other-window)
;; overridden by P4 commands. ugh.

;; src-grep commands
(define-key ctl-z-map "a" 'bc-co-grep)
(define-key ctl-z-map "i" 'bc-dev-grep)
(define-key ctl-z-map "l" 'aron-grep)

;; move the line with the cursor to the top of the frame.
(define-key ctl-z-map "t" 'line-to-top-of-window)

;;(define-key ctl-z-map "\C-f" 'cds-find-file-other-reusing-window)

;; this is probably a bit too aggressive.
;;(define-key global-map "\C-c" 'aron-compile)

(add-hook 'java-mode-hook (function (lambda ()
				      (local-set-key "\C-c\C-c" 'aron-compile))))


(provide 'aron-keys)
;;; aron-keys.el ends here
