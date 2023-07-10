;; lsp-mode-config.el
;;
;; lsp-mode for use with elixir
;;
;; Walter McGinnis, 2022-05-18
;;
;; http://github.com/walter/emakit

(use-package lsp-mode
  :commands lsp
  :ensure t
  :diminish lsp-mode
  :hook ((go-mode . lsp)
         (ruby-mode . lsp)
         (java-mode . lsp)
         (elixir-ts-mode . lsp)
         (heex-ts-mode . lsp)
         (typescriptreact-mode . lsp)
         (scala-mode . lsp))
  ;:init
  ;(add-to-list 'exec-path "~/.elixir-ls/release")
  ;(setq lsp-keymap-prefix "C-, l")
  :custom
  (lsp-lens-enable nil)
  (lsp-diagnostics-flycheck-default-level 'warning)
  (lsp-diagnostics-provider :flycheck)
  )

(use-package lsp-ui
  :requires lsp-mode flycheck
  :config
  (setq lsp-ui-doc-enable nil

        ;; fix diagnostics getting truncated in sideline
        ;; https://elixir-lang.slack.com/archives/C067Y5FN1/p1667322422740149
        ;; and https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
        lsp-ui-sideline-show-diagnostics nil
	lsp-ui-sideline-show-code-actions nil
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui)


;; lsp-file-watch-ignore-list
;; thanks aw from https://elixir-lang.slack.com/archives/C067Y5FN1/p1667383339645819
(dolist (match
         '("[/\\\\].direnv$"
           "[/\\\\]node_modules$"
           "[/\\\\]deps"
           "[/\\\\]priv"
           "[/\\\\]build"
	   "[/\\\\]\\.git$"
	   "[/\\\\]\\.bloop$"
	   "[/\\\\]\\.metals$"
           "[/\\\\]_build"))
  (add-to-list 'lsp-file-watch-ignored match))

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

;; https://github.com/emacs-lsp/lsp-treemacs
(use-package lsp-treemacs)

(lsp-treemacs-sync-mode 1)

;; lsp-credo requires lsp-mode 20230628.1609 or later
;; https://github.com/elixir-tools/credo-language-server
(custom-set-variables '(lsp-credo-version "0.2.0"))

(provide 'lsp-config)
