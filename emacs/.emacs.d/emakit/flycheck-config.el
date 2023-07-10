;; flycheck-config.el
;;
;; Install flycheck package for use with lsp-ui, etc.
;;
;; Walter McGinnis, 2023-06-19
;;
;; http://github.com/walter/emakit

(use-package flycheck
  :bind (("C-c C-n" . flycheck-next-error)
         ("C-c C-p" . flycheck-previous-error)))

(provide 'flycheck-config)
