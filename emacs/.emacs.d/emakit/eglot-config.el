;; lsp-mode-config.el
;;
;; eglot language server configuration


(use-package eglot
  :config (add-to-list 'eglot-server-programs '(elixir-ts-mode "elixir-ls"))
  :hook (elixir-ts-mode . eglot-ensure)
  )

(provide 'eglot-config)
