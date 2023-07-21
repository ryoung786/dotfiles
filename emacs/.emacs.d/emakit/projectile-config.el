(use-package projectile
  :delight projectile-mode
  :config
  (projectile-mode)
  (setq projectile-project-root-files-bottom-up
        (cons "mix.exs" projectile-project-root-files-bottom-up))
  :bind-keymap ("C-c p" . projectile-command-map)
  :init (setq projectile-switch-project-action #'projectile-dired))

(use-package ripgrep)

(provide 'projectile-config)
