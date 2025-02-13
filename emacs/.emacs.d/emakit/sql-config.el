(use-package emacs
  :custom
  (sql-postgres-login-params '(user database server port))
  :init
  (add-hook 'sql-mode-hook (lambda () (setq tab-width 2))))

(provide 'sql-config)
