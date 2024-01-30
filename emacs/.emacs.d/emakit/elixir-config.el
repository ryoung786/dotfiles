(require 'treesit)

(add-to-list 'major-mode-remap-alist '(elixir-mode . elixir-ts-mode))

(use-package elixir-mode)

(use-package elixir-ts-mode
  :hook
  (elixir-ts-mode . mix-minor-mode)
  (elixir-ts-mode . exunit-mode)
  ;; (elixir-ts-mode . (lambda () (add-hook 'before-save-hook 'eglot-format nil 'local)))
  :config (global-subword-mode t)
  :delight subword-mode)

(use-package heex-ts-mode
  :hook
  (heex-ts-mode . display-line-numbers-mode)
  (heex-ts-mode . git-gutter-mode)
  (heex-ts-mode . (lambda () (indent-tabs-mode -1)))
  ;; (heex-ts-mode . (lambda () (add-hook 'before-save-hook 'eglot-format nil 'local)))
  :init (add-to-list 'auto-mode-alist '("\\.[hl]?eex\\'" . heex-ts-mode)))

(use-package mix
  :delight mix-minor-mode
  :after elixir-ts-mode
  :config (add-hook 'elixir-ts-mode-hook 'mix-minor-mode))

(setq compilation-scroll-output t)

(use-package exunit
  :delight exunit-mode
  :after elixir-ts-mode
  :bind
  (:map elixir-ts-mode-map
        ("C-c , a" . exunit-verify-all)
        ("C-c , A" . exunit-verify-all-in-umbrella)
        ("C-c , s" . exunit-verify-single)
        ("C-c , v" . exunit-verify)
        ("C-c , r" . exunit-rerun)
        ("C-c , t" . exunit-toggle-file-and-test)
        ("s-r" . exunit-rerun)
        ))

(provide 'elixir-config)
