(require 'compile)
(require 'aron-grep)

(defun aron/is-connect-root (dir)
  "Return true if this directory looks like the RStudio Connect
repository root"
  (and
   (file-exists-p (concat dir "src"))
   (file-exists-p (concat dir "connect.Rproj"))
   ))

(defun aron/connect-root ()
  "Return the directory containing the RStudio Connect repository root."
  (locate-dominating-file default-directory 'aron/is-connect-root))

(defun aron/go-compile (&optional arg target)
    "Runs RStudio Connect compile.

If called with a non-nil ARG, the compile command is presented
for editing before it is executed."
  (interactive "P")
  (let* ((default-directory (aron/connect-root))
         (target (or target "build"))
         (make-command (concat "make " target)))
    (compile
     (if arg
         (read-from-minibuffer "make command: " make-command)
       make-command))))

(defun aron/find-go-package ()
  "Finds the name of the current Go package."
  (let* ((connect-root (aron/connect-root))
         (src-root (concat connect-root "src/"))
         (package-path (file-relative-name (string-remove-suffix "/" default-directory) src-root)))
    package-path
    ))

(defun aron/go-test (&optional arg)
  "Runs RStudio Connect test compile.

If called with a non-nil ARG, the compile command is
presented for editing before it is executed."
  (interactive "P")
  (let* ((package-path (aron/find-go-package))
         (connect-root (aron/connect-root))
         ;;(make-command (concat "make -C " connect-root " test-verbose TEST=" package-path " TEST_ARGS=")))
         (make-command (concat "make -C " connect-root " test-verbose TEST=" package-path)))
    ;; go test emits only the package-local path on errors
    (compile
     (if arg
         (read-from-minibuffer "make command: " make-command)
       make-command))))
(defalias 'aron/test-go 'aron/go-test)

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

(defun aron/selenium-test (&optional arg)
  "Runs RStudio Connect selenium tests.

If called with a non-nil ARG, the compile command is
presented for editing before it is executed."
  (interactive "P")
  (let* ((connect-root (aron/connect-root))
         (selenium-root (file-name-as-directory (concat (aron/connect-root) "test/selenium")))
         (selenium-test-file (file-relative-name (buffer-file-name) selenium-root))
         (make-command (concat "make -C " selenium-root " PYTESTOPTS=" selenium-test-file)))
    (compile
     (if arg
         (read-from-minibuffer "make command: " make-command)
       make-command))))
(defalias 'aron/test-selenium 'aron/selenium-test)

(defun aron/api-test (&optional arg)
  "Runs RStudio Connect API tests.

If called with a non-nil ARG, the compile command is
presented for editing before it is executed."
  (interactive "P")
  (let* ((connect-root (aron/connect-root))
         (api-root (file-name-as-directory (concat (aron/connect-root) "docs/api")))
         (api-test-file (file-relative-name (buffer-file-name) api-root))
         (make-command (concat "make -C " api-root " test NOSETESTSOPTS=" api-test-file)))
    (compile
     (if arg
         (read-from-minibuffer "make command: " make-command)
       make-command))))
(defalias 'aron/test-api 'aron/api-test)

(provide 'aron-compile)
