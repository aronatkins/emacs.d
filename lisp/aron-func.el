;;; package ---  Aron's utility functions

;;; Commentary:
;;; You know.

;;; Code:

(require 'thingatpt)

;; stolen from xemacs.
(defun backward-other-window (arg &optional all-frames)
  "Select the ARG'th different window on this frame, going backwards.
This is just like calling `other-window' with the arg negated."
  (interactive "p")
  (other-window (- arg) all-frames))

;; Kills an entire line.  Useful for when you want to delete many
;; lines, but don't feel like moving to position 0 first
(defun my-kill-whole-line ()
  "Kills current line."
  (interactive)
  (clipboard-kill-region 
   (progn
    (beginning-of-line)
    (point))
   (progn
	  (end-of-visible-line)
    (forward-char 1)
	  (point))))

;; Mark the current file executable
;; could also use executable-make-buffer-file-executable-if-script-p
(defun chmod-executable ()
  (interactive)
  (shell-command (concat "chmod +x " (buffer-file-name))))

;; chmod's the file to be writable, then reloads the buffer
(defun chmod-writable()
  (interactive)
  (shell-command (concat "chmod +w " (buffer-file-name)))
  (revert-buffer t t))

;; Converts all \r\n to \n for the current buffer
(defun dos-to-unix ()
  "Convert a DOS buffer to Unix format."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r\n" nil t)
    (replace-match "\n" nil t)))

;; http://www.blogbyben.com/2013/09/emacs-function-humanifying-urls.html
(defun url-humanify ()
  "Take the URL at point and make it human readable."
  (interactive)
  ;; ACA: bounds-of-thing-at-point doesn't do what we want if there
  ;; are unescaped characters in the URL, even if the browser is able
  ;; to handle it.
  ;; https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURIComponent
  ;; the ' character is a problem in graphite urls.
  (let* ((area (bounds-of-thing-at-point 'url))
         ;; count-occurances-in-region -> count-matches
         (num-params  (count-matches "&" (car area) (cdr area)))
         (i 0))
    (beginning-of-thing 'url)
    (when (search-forward "?" (cdr area) t nil)
      (insert "\n  ")
      (while (< i num-params)
        (search-forward "&" nil t nil)
        (insert "\n  ")
        (save-excursion
          (previous-line)
          (beginning-of-line)
          (let ((start (search-forward "="))
                (end (search-forward "&")))
            (url-decode-region start end)))
        (setq i (+ i 1))))))

(defun url-decode-region (start end)
  "Replace a region with the same contents, only URL decoded."
  (interactive "r")
  (let ((text (url-unhex-string (buffer-substring start end))))
    (delete-region start end)
    (insert text)))

;; from: http://emacsredux.com/blog/2013/05/30/joining-lines/
(defun top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))

;; from: http://emacswiki.org/emacs/CompileCommand
(defun aron/in-directory (dir)
  "Runs execute-extended-command with default-directory set to
the given directory."
  (interactive "DIn directory: ")
  (let ((default-directory dir))
    (call-interactively 'execute-extended-command)))

(defun aron/compile-in-directory (dir)
  "Runs execute-extended-command with default-directory set to
the given directory."
  (interactive "DIn directory: ")
  (let ((default-directory dir))
    (call-interactively 'compile)))

;; from: http://whattheemacsd.com/file-defuns.el-01.html
(defun aron/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

; http://emacswiki.org/emacs/DeletingWhitespace#toc18
; adapted from `delete-horizontal-space'
(defun delete-horizontal-space-forward ()
  "*Delete all spaces and tabs after point."
  (interactive "*")
  (delete-region (point) (progn (skip-chars-forward " \t") (point))))

;; http://stackoverflow.com/questions/5823495/emacs-how-to-yank-the-last-yanked-text-regardless-of-subsequent-kills
(defun aron/yank (&optional arg)
  "Yank and save text to register Y"
  (interactive)
  (set-register ?Y (current-kill 0 t))
  (yank arg))

(defun aron/yank-pop (&optional arg)
  "If yank-pop fails, then insert register Y"
  (interactive)
  (condition-case nil
      (yank-pop arg)
    (error (insert (get-register ?Y)))))

;; launch an independent emacs instance (mac)
;; https://www.emacswiki.org/emacs/MacOSTweaks#toc17
(when (memq window-system '(mac ns))
  (defun new-emacs ()
    (interactive)
    (shell-command "open -n -a /Applications/Emacs.app")))

(provide `aron-func)
;;; aron-func.el ends here
