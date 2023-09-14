(use-package projectile
  :delight projectile-mode
  :commands (projectile-project-name)
  :bind-keymap ("C-c p" . projectile-command-map)
  :init (setq projectile-switch-project-action #'projectile-dired))

(use-package project
  :custom (project-prompter 'project-prompt-project-name))

(use-package ripgrep)

(provide 'projectile-config)
