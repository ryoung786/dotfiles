(use-package projectile
  :delight projectile-mode
  :commands (projectile-project-name)
  :bind-keymap ("C-c p" . projectile-command-map)
  :init (setq projectile-switch-project-action #'projectile-dired))

(use-package project
  :custom (project-prompter 'project-prompt-project-name)
  :config (add-to-list 'project-switch-commands '(magit-project-status "Magit" 109) 'append)
  :bind (:map project-prefix-map ("m" . magit-project-status))
  )

(use-package ripgrep)

(provide 'projectile-config)
