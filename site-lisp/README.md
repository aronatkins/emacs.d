# site-lisp

Vendored packages that are not available through normal package installation.

## copilot.el

<https://github.com/copilot-emacs/copilot.el>

```bash
brew install node

curl -O https://raw.githubusercontent.com/copilot-emacs/copilot.el/main/copilot-balancer.el
curl -O https://raw.githubusercontent.com/copilot-emacs/copilot.el/main/copilot.el
```

M-x copilot-install-server


## templ-mode

https://github.com/federicotdn/templ-mode
https://raw.githubusercontent.com/federicotdn/templ-mode/refs/heads/main/templ-mode.el

```lisp
;; dir-locals.el to use `go tool templ`
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html
((templ-mode . ((templ-mode-command . tool))))
```
