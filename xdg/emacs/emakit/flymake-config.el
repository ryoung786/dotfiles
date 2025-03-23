;; flycheck-config.el
;;
;; Install flycheck package for use with lsp-ui, etc.
;;
;; Walter McGinnis, 2023-06-19
;;
;; http://github.com/walter/emakit

(use-package flymake
  :bind
  (:map flymake-mode-map
        ("C-c C-n" . flymake-goto-next-error)
	("C-c C-p" . flymake-goto-prev-error)))

(provide 'flymake-config)
