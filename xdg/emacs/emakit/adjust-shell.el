;; adjust-shell.el
;;
;; set shell to zsh
;;
;; Walter McGinnis, 2022-05-18
;;
;; http://github.com/walter/emakit

;; set default shell to zsh
;; assumes homebrew installed zsh at /opt/local/bin/zsh
;; as on Apple Silicon, but comments have /usr/local/bin/zsh
;; for older default
;; alternatively you can use zsh that comes with Mac OS X
;;
(setenv "ESHELL" "/bin/zsh")
(setenv "SHELL" "/bin/zsh")

;; if using emacs-plus, may not be necessary
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :init
  (progn (exec-path-from-shell-initialize)))

(use-package mise
  :delight mise-mode
  :init (add-hook 'after-init-hook #'global-mise-mode))

(provide 'adjust-shell)
