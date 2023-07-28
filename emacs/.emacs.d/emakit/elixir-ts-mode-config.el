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
  ((elixir-ts-mode . smartparens-mode)
   (elixir-ts-mode . mix-minor-mode)
   (elixir-ts-mode . exunit-mode)
   (elixir-ts-mode . (lambda () (add-hook 'before-save-hook 'elixir-format nil 'local))))
  :custom-face
  (elixir-ts-font-comment-doc-attribute-face ((t (:inherit font-lock-doc-face :slant normal))))
  (elixir-ts-font-comment-doc-face ((t (:inherit font-lock-doc-face :height 1.1))))
  (elixir-ts-font-comment-doc-identifier-face ((t (:inherit font-lock-doc-face :slant normal))))
  (elixir-ts-font-comment-unused-face ((t (:foreground "bisque3"))))
  (heex-ts-font-attribute-face ((t (:inherit font-lock-variable-name-face :foreground "LightSalmon1" :slant italic :family "Fira Code iScript"))))
  :config
  (global-subword-mode t)
  :delight subword-mode)

(provide 'elixir-ts-mode-config)
