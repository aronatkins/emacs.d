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

(defun aron/is-connect-module-root (dir)
  "Return true if this directory looks like an RStudio Connect
module root."
  ; This is a pretty terrible heursitic and assumes that packages in
  ; src/*/vendor do not use just.
  (and
   (file-exists-p (concat dir "go.mod"))
   (file-exists-p (concat dir "justfile"))
   ))

(defun aron/connect-module-root ()
  "Return the directory containing the root of an RStudio Connect
module hierarchy.

Typically, these are directories beneath connect/src."
  (locate-dominating-file default-directory 'aron/is-connect-module-root))

(defun aron/go-compile (&optional arg target)
    "Runs RStudio Connect compile.

If called with a non-nil ARG, the compile command is presented
for editing before it is executed."
  (interactive "P")
  (let* ((default-directory (aron/connect-root))
         (target (or target "build"))
         (make-command (concat "just " target)))
    (compile
     (if arg
         (read-from-minibuffer "make command: " make-command)
       make-command))))

(defun aron/go-test (&optional arg)
  "Runs RStudio Connect test compile.

If called with a non-nil ARG, the compile command is
presented for editing before it is executed."
  (interactive "P")
  (let* (
         ;; ~/dev/rstudio/connect/
         (connect-root (aron/connect-root))
         ;; ~/dev/rstudio/connect/src/
         (src-root (concat connect-root "src/"))
         ;; ~/dev/rstudio/connect/src/connect/
         (module-root (aron/connect-module-root))
         ;; connect
         (module-root-name (string-remove-suffix "/" (file-relative-name module-root src-root)))
         ;; connect/util
         (relative-package-path (string-remove-suffix "/" (file-relative-name default-directory src-root)))
         ;; either "test" or "test-tool"
         (target (if (string-equal "connect" module-root-name) "test" "test-tool"))
         (compile-command (concat "just " connect-root " " target " " relative-package-path))
         )
    ;; go test emits only the package-local path on errors
    (compile
     (if arg
         (read-from-minibuffer "compilation command: " compile-command)
       compile-command))))

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

(defun aron/connect-r-test (&optional arg)
  "Runs RStudio Connect R tests.

If called with a non-nil ARG, the compile command is
presented for editing before it is executed."

  (interactive "P")
  (let* ((connect-root (aron/connect-root))
         (make-command (concat "make -C " connect-root " test-r")))
    (compile
     (if arg
         (read-from-minibuffer "make command: " make-command)
       make-command))))

(provide 'aron-compile)
