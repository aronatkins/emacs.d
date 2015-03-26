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

(global-set-key (kbd "C-^") 'top-join-line)

;; use C-z as a user-meta key, since almost everything else is used!
(defvar ctl-z-map (make-sparse-keymap) "Keymap for user extensions.")

(define-key ctl-z-map "b" 'bury-buffer)
(define-key ctl-z-map "c" 'comment-or-uncomment-region)
(define-key ctl-z-map "e" 'er/expand-region)
(define-key ctl-z-map "f" 'flush-lines )
(define-key ctl-z-map "g" 'goto-line)
(define-key ctl-z-map "h" 'aron-jdk-help)
(define-key ctl-z-map "k" 'copy-region-as-kill)
(define-key ctl-z-map "l" 'aron-grep)
(define-key ctl-z-map "m" 'magit-status)
(define-key ctl-z-map "q" 'query-replace)
(define-key ctl-z-map "r" 'replace-string)
(define-key ctl-z-map "t" 'line-to-top-of-window)
(define-key ctl-z-map "v" 'revert-buffer)
(define-key ctl-z-map "w" 'widen)

;; since we're overloading C-z, we need to add suspend back into the
;; fray. we use C-z C-z
(define-key ctl-z-map "\C-z" 'suspend-emacs)

(global-set-key "\C-z" ctl-z-map)

;; key binding conventions:
;; Reserved for users:
;;   C-c [anything]
;; Reserved for major/minor modes:
;;   C-c C-anything
;;   C-c [digits]
;;   c-c [punctuation]
;; http://web.psung.name/emacs/2009/part2.html
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html

(global-set-key "\C-ct" 'toggle-truncate-lines)

;; define this because it works well with "C-x o"
(define-key ctl-x-map "p" 'backward-other-window)
;; overridden by P4 commands. ugh.

(add-hook 'java-mode-hook (function (lambda ()
				      (local-set-key "\C-c\C-c" 'aron-compile))))


(provide 'aron-keys)
;;; aron-keys.el ends here
