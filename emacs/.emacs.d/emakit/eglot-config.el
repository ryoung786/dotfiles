;; lsp-mode-config.el
;;
;; eglot language server configuration

(defun toggle-eldoc-doc-buffer ()
  "If the eldoc buffer is in a window, quit it.  If not, open it with eldoc-doc-buffer"
  (interactive)
  (if (get-buffer-window eldoc--doc-buffer)
      (quit-windows-on eldoc--doc-buffer)
    (with-current-buffer eldoc--doc-buffer (display-buffer (current-buffer)))))

(use-package eglot
  :config
  ;; (add-to-list 'eglot-server-programs `((elixir-ts-mode heex-ts-mode elixir-mode) . ("~/dev/language_servers/elixir/lexical/_build/dev/package/lexical/bin/start_lexical.sh")))
  ;; (add-to-list 'eglot-server-programs '(elixir-ts-mode "elixir-ls"))
  (add-to-list 'eglot-server-programs `((elixir-ts-mode heex-ts-mode elixir-mode) . "elixir-ls"))
  ;; (add-to-list 'eglot-server-programs `((elixir-ts-mode heex-ts-mode elixir-mode) . ("~/dev/language_servers/elixir/next-ls/bin/start" "--stdio=true")))
  :hook ((elixir-ts-mode . eglot-ensure) (heex-ts-mode . eglot-ensure)))

;; (use-package ghelp
;;   :bind (:map prog-mode-map ("C-h ." . ghelp-describe)))

(provide 'eglot-config)
