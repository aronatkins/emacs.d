(require 'compile)
(require 'aron-grep)

;; -----------------------------------------------------------------------------
;; Variables you can override 
;;
(defvar bc-ant 
  "ant"
  "The ant you use for compilation. By default, we use whatever is in your path.")

(defvar bc-ant-args 
  "-emacs"
  "Default arguments to pass to ant when running `bc-compile'. The
'-emacs' switch is very helpful, as it makes ant output friendly to
the emacs `compile-mode'.")

(defvar bc-mvn
  "mvn"
  "The mvn you use for compilation. By default, we use whatever is in your path.")

;; send -q to prevent mvn from sending download percent-complete messages, which 
;; causes emacs to use 100%cpu. ugh.
(defvar bc-mvn-args 
  "-q"
  "Default arguments to pass to mvn when running `bc-compile'.")

;; add support for maven error statements.
(add-to-list
 'compilation-error-regexp-alist
 '("^\\([a-zA-Z]:.*\\|/[a-zA-Z].*\\):\\[\\([0-9]+\\),\\([0-9]+\\)\\]" 1 2 3)) 

(defun bc-at-co-root (path)
  "Boolean. Does 'path' look like the top of a checkout?"
  ;;(and (file-exists-p (concat path "tools"))
  ;;     (file-exists-p (concat path "dev"))
  (and (file-exists-p (concat path "dev"))
       (file-exists-p (concat path "dev/origin"))
       ;;(file-exists-p (concat path "test"))
       ))

(defun bc-parent-directory (dir)
  "Return the parent directory of the provided directory."
  (file-name-directory (directory-file-name dir)))

(defun bc-relative-directory (dir)
  "Return the final chunk of a path."
  (file-relative-name dir (bc-parent-directory dir)))

;; TODO: rewrite this as two parts: 
;; 1. generate a list of recursive parent directories
;; 2. filter that list and return the first element which passes the callback.
(defun bc-find-matching-directory (callback)
  "Helper to find a directory matching some pattern (searches UP from current dir)."
  (let ((matching-directory)
	(at-root-directory)
	(current-directory default-directory))
    (while (and current-directory (not (or matching-directory at-root-directory)))
      (let ((parent-directory (bc-parent-directory current-directory)))
	;; look for a matching directory
	;;(message (concat "Testing: " current-directory))
	(if (funcall callback current-directory)
	    (setq matching-directory current-directory))
	;; prepare for next iteration
	(setq at-root-directory (string-equal current-directory parent-directory))
	;;(message (concat "Current: " current-directory))
	;;(message (concat "Parent:  " parent-directory))
	(setq current-directory parent-directory)))

    (if matching-directory
        matching-directory
      nil)))

(defun bc-find-co-root ()
  "Try and find the root of a Brightcove Perforce checkout. If it cannot be found,
return nil."
  (bc-find-matching-directory 'bc-at-co-root))

(defun bc-find-co-component (component)
  "Helper to find a 'component' of a checkout: dev, tomcat, usr-local, etc."
  (let ((co-root (bc-find-co-root)))
    (if co-root
	(let ((component-root (concat co-root component)))
	  (if (file-exists-p component-root)
	      component-root)))))

(defun bc-find-dev-root ()
  "Try and find the root of a Brightcove Perforce source tree. If it cannot be found,
return the current directory of the active buffer."
  (bc-find-co-component "dev/"))

(defun bc-is-tests-directory (dir)
  "return true if the name of this directory looks like it contains test code. useful because origin/tests contains a build.xml (legacy)."
  (string-equal (bc-relative-directory dir) "tests/"))

(defun bc-is-mvn-dir (dir)
  "return true if this looks like a directory built with mvn (and not origin)."
  ;; don't use mvn in origin yet.
  (and (not (string-equal "origin" (file-name-nondirectory (directory-file-name dir))))
       (file-exists-p (concat dir "pom.xml"))))

(defun bc-is-ant-dir (dir)
  "return true if this looks like a directory built with ant."
  (file-exists-p (concat dir "build.xml")))

(defun bc-is-compile-directory (dir)
  "return true if this looks like a directory that can be build with either mvn or ant."
  (or (bc-is-mvn-dir dir) (bc-is-ant-dir dir)))

(defun bc-find-build-root ()
  "Try and find the directory where a build should be performed. An
appropriate directory is determined by the presence of a build.xml
file. If a match could not be found, return the current directory." 
  (bc-find-matching-directory 
   (function (lambda (dir) (and (bc-is-compile-directory dir)
                                ;; don't stop at test directories;
                                ;; even if there is a build.xml.
                                (not (bc-is-tests-directory dir)))))))

(defvar bc-compile-command-history 
  nil 
  "Internal state for `bc-compile'; do not modify.")

(defun bc-compile-ant-command (build-root &optional target)
  (concat "cd " build-root "; " bc-ant " " bc-ant-args " " (or target "")))

(defun bc-compile-mvn-command (build-root &optional target)
  (concat "cd " build-root "; " bc-mvn " " bc-mvn-args " " (or target "")))

(defun bc-compile-command (&optional target)
  "Return a compile-command."
  (let ((build-root (or (bc-find-build-root) default-directory)))
    ;; prefer mvn building to ant. 
    (if (bc-is-mvn-dir build-root)
        (bc-compile-mvn-command build-root target)
      (bc-compile-ant-command build-root target))))

(defun bc-compile (command)
  "Perform a compile."
  (interactive "P")
  (compile (read-from-minibuffer "Compile command: " 
                                 (or command (bc-compile-command))
                                 nil nil 'bc-compile-command-history)))

(defun bc-local-test-path (path)
  "Remove everything before the com/brightcove/..."
  ;; our old ant projects use "test/junit" or "tests/src/junit" as a
  ;; root; mvn uses test/java. everything after the "junit" or "java"
  ;; is the package+class.
  (replace-regexp-in-string "^.*/\\(junit\\|java\\)/" "" path))

(defun bc-mvn-test-arguments (path)
  "build mvn arguments to execute a single test."
  (concat "-Dsurefire.useFile=false" " " "-Dtest=" (bc-local-test-path path) " " "test"))

; Determine the -D argument whether or not we're in origin or not.
(defun bc-ant-test-arguments (path)
  "Based on the path determine which ant var to use to specify a test to run."
  (concat 
   (if (string-match "origin/tests/src/junit" path)
       "tests-compile tests-run-fast -Dtests.singletest="
     "compile test -Dtest.name=")
   (bc-local-test-path path)))

(defun bc-test ()
  "Perform a test run on a single file. Run from a buffer
containing a test file; builds ant/mvn command
line to run just that single test."
  (interactive)
  (let ((build-root (or (bc-find-build-root) default-directory)))
    ;; prefer mvn building to ant. 
    (bc-compile 
     (if (bc-is-mvn-dir build-root)
         (bc-compile-mvn-command build-root (bc-mvn-test-arguments (buffer-file-name)))
       (bc-compile-ant-command build-root (bc-ant-test-arguments (buffer-file-name)))))))

(defun bc-co-grep ()
  "Call grep with a find/xargs pipe. Wrapper around `aron-grep'.
Attempts to root the search at the top of a checkout."
  (interactive)
  (aron-grep (bc-find-co-root)))

(defun bc-dev-grep ()
  "Call grep with a find/xargs pipe. Wrapper around `aron-grep'.
Attempts to root itself in the root of the source tree (not the checkout root)."
  (interactive)
  (aron-grep (bc-find-dev-root)))

(defun aron/is-connect-root (dir)
  "return true if this directory looks like the connect root"
  (and
   (file-exists-p (concat dir "src"))
   (file-exists-p (concat dir "pkg"))
   (file-exists-p (concat dir "Makefile.docker"))
   ))

(defun aron/find-go-root ()
  "return the Connect root"
  (bc-find-matching-directory 'aron/is-connect-root))

(defun aron/go-compile (&optional arg target)
    "Runs RStudio Connect compile.

If called with a non-nil ARG, the compile command is presented
for editing before it is executed."
  (interactive "P")
  (let* ((default-directory (aron/find-go-root))
         (target (or target "build"))
         (make-command (concat "make -f Makefile.docker " target)))
    (compile
     (if arg
         (read-from-minibuffer "make command: " make-command)
       make-command))))

(defun aron/find-go-package ()
  "Finds the name of the current Go package."
  (let* ((go-root (aron/find-go-root))
         (src-root (concat go-root "src/"))
         (package-path (string-remove-prefix src-root (string-remove-suffix "/" default-directory))))
    package-path
    ))

(defun aron/go-test (&optional arg)
  "Runs RStudio Connect test compile.

If called with a non-nil ARG, the compile command is
presented for editing before it is executed."
  (interactive "P")
  (let* ((package-path (aron/find-go-package))
         (go-root (aron/find-go-root))
         ;;(make-command (concat "make -C " go-root " -f Makefile.docker test-verbose TEST=" package-path " TEST_ARGS=")))
         (make-command (concat "make -C " go-root " -f Makefile.docker test-verbose TEST=" package-path)))
    ;; go test emits only the package-local path on errors
    (compile
     (if arg
         (read-from-minibuffer "make command: " make-command)
       make-command))))

;; This doesn't work. Killing the compile with C-c C-k "kills" it differently
;; than a Ctrl-C in a terminal. In particular, the docker instance is left
;; running and license deregistration never fires!! So. Stay away.
;;
;; Things work better when running "dmake start" inside a shell, as a C-c C-c
;; sends a Ctrl-C that is handled as in a terminal.
(defun aron/go-start (&optional arg)
    "Runs RStudio Connect.
"
  (interactive "P")
  (aron/go-compile arg "start"))

(provide 'bc-compile)
