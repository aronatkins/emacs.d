;;; aron-keys.el --- Key bindings  -*- lexical-binding: t; -*-

;;; Commentary:
;; Personal key bindings and keyboard customizations.

;;; Code:

(require 'aron-func)
(require 'aron-grep)
(require 'aron-compile)
(require 'crux)

;; alter C-y and M-y so M-y uses the last yank instead of erring.
;;(global-set-key (kbd "M-y") (quote aron/yank-pop))
;;(global-set-key (kbd "C-y") (quote aron/yank))

;; fix the backspace problems on many terminals
;(global-set-key "\C-x\C-h" 'help-command)
;(global-set-key "\C-h" 'delete-backward-char)
;(global-set-key "\C-?" 'delete-char)

(global-set-key (kbd "C-^") 'top-join-line)

;; use C-z as a user-meta key, since almost everything else is used!
(defvar ctl-z-map (make-sparse-keymap) "Keymap for user extensions.")

(define-key ctl-z-map "b" 'bury-buffer)
;; C-x C-; can also toggle commenting on line-or-region.
(define-key ctl-z-map "c" 'comment-or-uncomment-region)
(define-key ctl-z-map "d" 'aron/compile-in-directory)
(define-key ctl-z-map "e" 'er/expand-region)
(define-key ctl-z-map "F" 'flush-lines)
(when (memq window-system '(mac ns))
  (define-key ctl-z-map "f" 'reveal-in-osx-finder))
(define-key ctl-z-map "g" 'goto-line)
(define-key ctl-z-map "i" 'aron/in-directory)
(define-key ctl-z-map "j" 'aron/git-grep)
(define-key ctl-z-map "J" 'aron/git-grep-cleanup)
(define-key ctl-z-map "k" 'copy-region-as-kill)
(define-key ctl-z-map "l" 'aron-grep)
(define-key ctl-z-map "m" 'magit-status)
(define-key ctl-z-map "o" 'crux-open-with)
(define-key ctl-z-map "q" 'query-replace)
(define-key ctl-z-map "r" 'replace-string)
(define-key ctl-z-map "s" 'aron/web-search)
(define-key ctl-z-map "v" 'revert-buffer)
(define-key ctl-z-map "w" 'widen)

;; since we're changing the default meaning of C-z, add suspend back into the
;; fray with C-z C-z.
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
;; balance-windows: Ctrl-x +
;; overridden by P4 commands. ugh.

(add-hook 'java-mode-hook (function (lambda ()
				      (local-set-key "\C-c\C-c" 'aron-compile))))

;; a better space collapsing binding.
;; http://pragmaticemacs.com/emacs/cycle-spacing/
(global-set-key (kbd "M-SPC") 'cycle-spacing)
;; and collapse space forward.
(define-key ctl-z-map " " 'delete-horizontal-space-forward)

;; Change text size!
;; C-x C-- / C-x C-+ / C-x C-0

;; Navigate around parens
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Moving-by-Parens.html
;; C-M-n C-M-p

(provide 'aron-keys)
;;; aron-keys.el ends here
