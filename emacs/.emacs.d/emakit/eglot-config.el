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
  :config (add-to-list 'eglot-server-programs '(elixir-ts-mode "elixir-ls"))
  :bind (:map eglot-mode-map ("C-h ." . toggle-eldoc-doc-buffer))
  :hook (elixir-ts-mode . eglot-ensure)
  )

(provide 'eglot-config)
