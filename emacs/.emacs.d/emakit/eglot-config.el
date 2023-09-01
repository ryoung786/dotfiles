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
  (add-to-list 'eglot-server-programs '(elixir-ts-mode "~/dev/lsps/elixir/lexical/_build/dev/package/lexical/bin/start_lexical.sh"))
  ;; (add-to-list 'eglot-server-programs '(elixir-ts-mode "elixir-ls"))
  ;; (add-to-list 'eglot-server-programs `((elixir-ts-mode heex-ts-mode elixir-mode) . ("~/dev/lsps/elixir/nextls" "--stdio=true")))
  :bind (:map eglot-mode-map ("C-h ." . toggle-eldoc-doc-buffer))
  :hook (elixir-ts-mode . eglot-ensure)
  )


;; (use-package quelpa
;;   :init
;;   (quelpa '(ghelp
;; 	    :fetcher git
;; 	    :url "https://github.com/casouri/ghelp.git")))

;; (use-package ghelp
;;   :bind (:map prog-mode-map ("C-h ." . ghelp-describe)))

(provide 'eglot-config)
