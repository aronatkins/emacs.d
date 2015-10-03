;;; package ---  Aron's dev stuff.

;;; Commentary:
;;; You know.

;;; Code:

;;(require 'aron-c-mode)
;;(add-hook 'c-mode-hook 'aron-c-mode)

;;(add-hook 'c-mode-hook '(lambda ()
;;			  (local-set-key "\C-c\C-c" 'compile)
;;;;			  (setq compile-command "make -k USERCFLAGS="-g -Wall")
;;			  (setq compile-command "cd /work/aron/build/play/Linux.debug && make -j8 all && make -j8 check")
;;			  )
;;	  )

(require 'gud)
(add-hook 'gdb-mode-hook 'aron-gdb-mode-hook)
(autoload 'aron-gdb-mode-hook "aron-gdb-hooks")

;; use c++-mode instead of c-mode for .h files.
(setq auto-mode-alist (cons '("\\.h\\'" . c++-mode) auto-mode-alist))

(add-hook 'c++-mode-hook '(lambda ()
			    (local-set-key "\C-c\C-c" 'compile)
			    (setq compile-command "cd `findsrc`/.. && remake")
			    ))


;; http://endlessparentheses.com/create-github-prs-from-emacs-with-magit.html
(defun aron/visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "https://github.com/%s/pull/new/%s"
           (replace-regexp-in-string
            "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
            (magit-get "remote"
                       (magit-get-remote)
                       "url"))
           (cdr (or (magit-get-remote-branch)
                    (user-error "No remote branch"))))))

(eval-after-load 'magit
  '(define-key magit-mode-map "v"
     #'aron/visit-pull-request-url))

(provide 'aron-dev)
;;; aron-dev.el ends here
