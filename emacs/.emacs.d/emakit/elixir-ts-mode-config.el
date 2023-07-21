;; elixir-ts-mode-config.el
;;
;; Install elixir-ts-mode package for use with lsp-mode
;;
;; Walter McGinnis, 2023-06-19
;;
;; http://github.com/walter/emakit

(require 'treesit)

(add-to-list 'major-mode-remap-alist '(elixir-mode . elixir-ts-mode))

(use-package elixir-mode)

(use-package elixir-ts-mode
  :hook
  (
   (elixir-ts-mode . smartparens-mode)
   (elixir-ts-mode . mix-minor-mode)
   (elixir-ts-mode . exunit-mode)
   (elixir-ts-mode . (lambda () (add-hook 'before-save-hook 'eglot-format-buffer nil 'local)))
   )
  :config
  (global-subword-mode t)
  :delight subword-mode)

(provide 'elixir-ts-mode-config)
