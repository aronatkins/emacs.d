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



(provide 'aron-dev)
;;; aron-dev.el ends here
