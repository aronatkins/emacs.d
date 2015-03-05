;;; package ---  What Aron likes.

;;; Commentary:
;;; You know.

;;; Code:

(require 'aron-func)

(defvar aron-grep-ignored-patterns  (list 
   "TAGS"
   "*~"
   "*.o"
   "*.dll"
   "*.class"
   "*.jar"
   "*.war"
   "*.orig"
   "*.pyc"
   "*.min.js"
   "#*#")
  "Patterns which are not ignored during a find/grep. Used by `aron-grep'
and its variants.")

(defun aron-build-grep-args-for-find ()
  "Combine the patterns in `aron-grep-ignored-patterns' into a legal set
of find arguments."
  (concat "-type f " (mapconcat (function
				 (lambda(pattern)
				   (concat "-and -not -name '" pattern "'")))
				aron-grep-ignored-patterns " ")))

(defvar aron-grep-args-for-find 
  (aron-build-grep-args-for-find)
  "User settable arguments (for find) used by `aron-grep' to
locate files that will be grep'ed. ")

(defvar aron-grep-args-for-grep "" 
  "User settable arguments (for grep) used by `aron-grep' to control the
grep command.")

(defvar aron-max-num-grep-windows 1 
  "User controllable number of grep windows that are kept.")

(defvar aron-grep-internal-constraint-history nil 
  "Internal variable for aron-grep; do not modify")

(defvar aron-grep-internal-symbol-history nil 
  "Internal variable for aron-grep; do not modify")

(defun aron-grep (&optional base-directory)
  "Call grep with a find/xargs pipe. If base-directory is non-nil, the
find/grep is rooted with that directory. If base-directory is
nil, roots the search at the default directory. Prompts the
user for each clause.

The embedded find and xargs options are specific to the GNU
implementations of each tool. In particular, they cause the pipe to
separate filenames with null characters instead of whitespace --
allowing for filenames with whitespace."
  (interactive "P")
  (let ((grep-directory (or base-directory default-directory))
	(temp-buffer-name-function compilation-buffer-name-function))
    (progn
      (setq compilation-buffer-name-function 'aron-compilation-buffer-name-function)
      (grep (concat "find "
		  (read-file-name "base directory: "
					grep-directory grep-directory t)
		  " "
		  (read-from-minibuffer "find constraint: "
					aron-grep-args-for-find
					nil nil 'aron-grep-internal-constraint-history)
		  " -print0 | xargs -0 grep -n "
		  (read-from-minibuffer "search for: "
					(concat (aron-grab-a-symbol) " " aron-grep-args-for-grep)
					nil nil 'aron-grep-internal-symbol-history)))
      (setq compilation-buffer-name-function temp-buffer-name-function)
      )))

(defun aron-compilation-buffer-name-function (mode)
  "if `aron-max-num-grep-windows' is > 1 then this function allows each
grep command to create up to that many buffers for grep. If other
compilation buffers are needed then this function does the same thing
that would happen if this function were not defined."
  (progn
    (if (and (equal (downcase mode) "grep") ;called for a grep buffer
	     (> aron-max-num-grep-windows 1) ;limited number of grep buffers
	     (get-buffer "*grep*"))	    ;a *grep* buffer exists
	(save-excursion
	  ;; count grep buffers, if more than limit then identify the oldest ones for removal
	  ;; We reserve the latest *grep* buffer in all cases
	  (let ((seen-grep-buffers 1))
	    (dolist (buffer (buffer-list))
	      (let ((name (buffer-name buffer)))
		(if (and (> (length name) 6) (equal (substring name 0 7) "*grep*<"))
		    (if (>= seen-grep-buffers (- aron-max-num-grep-windows 1))
			(kill-buffer name)
		      (setq seen-grep-buffers (+ 1 seen-grep-buffers)))))))
	  (if (get-buffer "*grep*")
	      (progn
		(set-buffer "*grep*")
		(rename-uniquely)))))
    (concat "*" (downcase mode) "*" )))

(provide 'aron-grep)
;;; aron-grep.el ends here
