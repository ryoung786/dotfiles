;; lsp-config.el
;;
;; Configuration of Language Server Provider (LSP) packages and options

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs
               `((elixir-ts-mode heex-ts-mode elixir-mode) . ,(eglot-alternatives
				    '(("~/dev/language_servers/elixir/lexical/_build/dev/package/lexical/bin/start_lexical.sh") ; lexical
				      ("elixir-ls")                                                                             ; elixir-ls
                                      ("/opt/homebrew/Cellar/next-ls/0.20.2/bin/nextls" "--stdio=true" :initializationOptions (:experimental (:completions (:enable t))))))))
  :hook
  (elixir-ts-mode . eglot-ensure)
  (heex-ts-mode . eglot-ensure))

(use-package lsp-mode
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion))

;; GHelp provides a uniform interface for eldoc and other help/documentation popups
;; It's not on MELPA though so until I install straight.el it'll stay commented out

;; (use-package ghelp
;;   :bind (:map prog-mode-map ("C-h ." . ghelp-describe)))

(defun toggle-eldoc-doc-buffer ()
  "If the eldoc buffer is in a window, quit it.  If not, open it with eldoc-doc-buffer"
  (interactive)
  (if (get-buffer-window eldoc--doc-buffer)
      (quit-windows-on eldoc--doc-buffer)
    (with-current-buffer eldoc--doc-buffer (display-buffer (current-buffer)))))

(provide 'lsp-config)
