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

(defun aron/go-module-root ()
  "Return the root directory of a Go module hierarchy.

Assumes that a vendored Go module does not include a go.mod file."
  (locate-dominating-file default-directory "go.mod"))

(defun aron/is-dashboard-root (dir)
  "Return true if this directory looks like it contains the code for the RStudio Connect dashboard."
  (file-exists-p (concat dir "package.json")))

(defun aron/dashboard-root ()
  "Return the directory containing the root of the RStudio Connect dashboard."
  (locate-dominating-file default-directory 'aron/is-dashboard-root))

(defun aron/dashboard-compile (&optional arg)
  "Runs RStudio Connect dashboard compilation.

If called with a non-nil ARG, the compile command is presented
for editing before it is executed."
  (interactive "P")
  (let* (
         (dashboard-root (aron/dashboard-root))
         (default-directory dashboard-root)
         (compile-command (concat "just " dashboard-root " build"))
         )
    (compile
     (if arg
         (read-from-minibuffer "compilation command: " compile-command)
       compile-command))))

(defun aron/go-compile (&optional arg)
  "Compiles a Go project with special awareness of the Posit Connect project.

When ARG is non-nil, the compile command is presented for editing before
it is executed."
  (interactive "P")
  (let*
      (
       ;; ~/dev/rstudio/connect/
       (connect-root (aron/connect-root))
       ;; ~/dev/rstudio/connect/src/connect/
       (module-root (aron/go-module-root))
       )
    (if connect-root
        (let*
            (
             ;; ~/dev/rstudio/connect/src/
             (src-root (concat connect-root "src/"))
             ;; connect
             (module-root-name (string-remove-suffix "/" (file-relative-name module-root src-root)))
             ;; either "server" or "tools"; use "build" to build both, but the
             ;; paths might not resolve correctly because of the default-directory
             ;; adjustment.
             (target (if (string-equal "connect" module-root-name) "server" "tools"))
             ;; setting the default-directory for the compile helps compilation error paths resolve
             ;; this is a hack, but we cd into the src/connect directory during the build.
             (default-directory module-root)
             (compile-command (concat "just " connect-root " " target))
             )
          (compile
           (if arg
               (read-from-minibuffer "compilation command: " compile-command)
             compile-command))
          )
      ;; if not connect-root
      (let*
          (
           (default-directory module-root)
           (compile-command "go build ./...")
           )
        (compile
         (if arg
             (read-from-minibuffer "compilation command: " compile-command)
           compile-command))
        )
      )
    )
  )

(defun aron/go-test (&optional arg)
  "Tests a Go p roject with special awareness of the Posit Connect project.

When ARG is non-nil, the compile command is presented for editing before
it is executed."
  (interactive "P")
  (let*
      (
       ;; ~/dev/rstudio/connect/
       (connect-root (aron/connect-root))
       ;; ~/dev/rstudio/connect/src/connect/
       (module-root (aron/go-module-root))
       )
    (if connect-root
        (let*
            (
             ;; ~/dev/rstudio/connect/src/
             (src-root (concat connect-root "src/"))
             ;; connect
             (module-root-name (string-remove-suffix "/" (file-relative-name module-root src-root)))
             ;; connect/util
             (relative-package-path (string-remove-suffix "/" (file-relative-name default-directory src-root)))
             ;; either "test" or "test-tool"
             (target (if (string-equal "connect" module-root-name) "test" "test-tool"))
             ;; Setting default-directory to module-root helps resolve compilation
             ;; errors but not test failures.
             ;;
             ;; Letting default-directory float to the directory with the current
             ;; Go file helps resolve test failures but not compilation errors.
             ;;
             ;; There is no good choice.
             (default-directory module-root)
             (compile-command (concat "just " connect-root " " target " " relative-package-path))
             )
          ;; go test emits only the package-local path on errors
          (compile
           (if arg
               (read-from-minibuffer "test command: " compile-command)
             compile-command))
          )
      ;; if not connect-root
      (let*
          (
           (default-directory module-root)
           (compile-command "go test ./...")
           )
        (compile
         (if arg
             (read-from-minibuffer "compilation command: " compile-command)
           compile-command))
        )
      )
    )
  )

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
  (let* (
         ;; ~/dev/rstudio/connect/
         (connect-root (aron/connect-root))
         (default-directory connect-root)
         (compile-command (concat "just " connect-root " start"))
         )
    (compile
     (if arg
         (read-from-minibuffer "start command: " compile-command)
       compile-command))))

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
         (compile-command (concat "just " connect-root " test-r")))
    (compile
     (if arg
         (read-from-minibuffer "command: " compile-command)
       compile-command))))

(defun aron/connect-packrat-test (&optional arg)
  "Runs RStudio Connect packrat tests (testing R, driven by Python).

If called with a non-nil ARG, the compile command is
presented for editing before it is executed."

  (interactive "P")
  (let* ((connect-root (aron/connect-root))
         (compile-command (concat "just " connect-root " test-packrat")))
    (compile
     (if arg
         (read-from-minibuffer "command: " compile-command)
       compile-command))))

(defun aron/connect-python-test (&optional arg)
  "Runs RStudio Connect Python tests.

If called with a non-nil ARG, the compile command is
presented for editing before it is executed."

  (interactive "P")
  (let* ((connect-root (aron/connect-root))
         (compile-command (concat "just " connect-root " test-python")))
    (compile
     (if arg
         (read-from-minibuffer "command: " compile-command)
       compile-command))))

(defun aron/eslint-executable ()
  ""
  (interactive)
  (let ((project-dir (locate-dominating-file default-directory "node_modules")))
    (unless project-dir
      (error "cannot locate node_modules"))
    (concat project-dir "node_modules/.bin/eslint")))

;; from https://gist.github.com/ustun/73321bfcb01a8657e5b8
(defun aron/eslint-fix-file (file-name)
  ""
  (interactive)
  (let* ((eslint-executable (aron/eslint-executable))
        (command (concat eslint-executable " --fix " file-name)))
    (message (concat "eslint: " command))
    (shell-command command)))

(defun aron/eslint-fix-file-docker (file-name)
  "Use the Connect dashboard Dockerfile to run eslint."
  (interactive)
  (let* ((default-directory (aron/dashboard-root))
         (command (concat "just docker-run-unsafe eslint -c eslint.config.mjs --fix " (file-relative-name file-name default-directory))))
    (message (concat "eslint-docker (from " default-directory "): " command))
    (shell-command command)
    ))

(defun aron/eslint-fix-file-and-revert ()
  ""
  (interactive)
  (let* ((connect-root (aron/connect-root)))
    (if connect-root
        (aron/eslint-fix-file-docker (buffer-file-name))
      (aron/eslint-fix-file (buffer-file-name))
      )
    (revert-buffer t t)))

(defun aron/packer-fix-file (file-name)
  "Run 'packer fmt' on the named file."
  (interactive)
  (let* ((command (concat "packer fmt " file-name)))
    (message (concat "formatting: " command))
    (shell-command command)))

(defun aron/packer-fix-file-and-revert ()
  "Run 'packer fmt' on the current buffer file and revert."
  (interactive)
  (aron/packer-fix-file (buffer-file-name))
  (revert-buffer t t))

(provide 'aron-compile)
