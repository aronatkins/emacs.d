;;; aron-treesitter.el --- Tree-sitter grammar configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; Defines tree-sitter grammar sources.
;; Run as script to install: emacs --script lisp/aron-treesitter.el

;;; Code:

(require 'treesit)

(setopt treesit-language-source-alist
      '((go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (just "https://github.com/IndianBoy42/tree-sitter-just")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (templ "https://github.com/vrischmann/tree-sitter-templ")
        (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")))

(defun aron/install-treesit-grammars ()
  "Install all tree-sitter grammars defined in `treesit-language-source-alist'."
  (dolist (lang (mapcar #'car treesit-language-source-alist))
    (unless (treesit-language-available-p lang)
      (message "Installing tree-sitter grammar for %s..." lang)
      (treesit-install-language-grammar lang)))
  (message "Tree-sitter grammars installed."))

;; Install grammars when run as a script.
(when noninteractive
  (aron/install-treesit-grammars))

(provide 'aron-treesitter)
;;; aron-treesitter.el ends here
