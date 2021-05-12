;;; gcfg-mode.el --- Major mode for editing .gcfg files.

;;; References:

;; https://github.com/Lindydancer/ini-mode
;; https://github.com/magit/git-modes

;;; Commentary:

;; A major mode for editing .gcfg files.

;;; Code:

(defvar gcfg-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; semicolon and hash comments
    ; (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\# "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for `gcfg-mode'.")

(setq gcfg-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; semicolon and hash comments
    ; (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\# "<" table)
    (modify-syntax-entry ?\n ">" table)
    table))

(defvar gcfg-font-lock-keywords
  '(("^\\[\\(.*\\)\\]"
     (1 font-lock-function-name-face))
    ("^\\([^ \t\n=]+\\) *="
     (1 font-lock-variable-name-face)))
  "Highlight rules for `gcfg-mode'.")

(defmacro gcfg-define-prog-mode (mode name &rest args)
  "Define major mode MODE for a programming language.
The mode will be named NAME and remaining arguments, ARGS, are
passed to `define-derived-mode'.
If `prog-mode' is defined, inherit from it."
  (declare (indent defun))
  `(define-derived-mode
     ,mode ,(and (fboundp 'prog-mode) 'prog-mode)
     ,name ,@args))

;;;###autoload(autoload 'gcfg-mode "gcfg-mode" nil t)
(gcfg-define-prog-mode gcfg-mode "gcfg"
  "Major mode for editing gcfg files."
  (setq font-lock-defaults '(gcfg-font-lock-keywords nil)))

;;;###autoload(add-to-list 'auto-mode-alist '("\\.gcfg\\'" . gcfg-mode))

(provide 'gcfg-mode)
