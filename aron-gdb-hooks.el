(require 'gud)
(require 'shell)

;; if you want to add other abbrevs just add other lines like the commented 
;; example note that the last two elements in must be nill 0
(defvar aron-gdb-mode-abbrev-table nil
  "Abbrev table used in GDB buffers.")
(define-abbrev-table 'aron-gdb-mode-abbrev-table '(
   ;; the following is an example that expands 'ex' to 'expanded text'
   ;;  ("ex"        "expanded text" nil 0)
   ;; here is where the real table starts:
   ("car"        "call carbonSomeLongFunction ( 0, " nil 0)
  )
)

(defun aron-gdb-mode-hook ()
  "customize gdb for use with code"
  ;; (setq comint-prompt-regexp "^\(gdb[+]*\) *")
  ;; (define-key gdb-mode-map "\e\t" 'comint-dynamic-complete)

  ;; make a local copy of syntax table, and allow '_' as word
  (set-syntax-table (copy-syntax-table (syntax-table))) 
  (modify-syntax-entry ?_ "w")
  (setq local-abbrev-table aron-gdb-mode-abbrev-table)
  (abbrev-mode 1)

  ;; have emacs pay attention to the cwd of gdb -- makes C-x C-f work the
  ;; same way as in a shell. -- note that we need a 'dirs' command (in
  ;; ~/.gdbinit)
  (add-hook 'comint-input-filter-functions 'shell-directory-tracker nil t)

)

;; (add-hook 'gdb-mode-hook 'aron-gdb-mode-hook)

;; don't 'cd' to the executable location
;;(setq gud-chdir-before-run nil)


(provide 'aron-gdb-hooks)
