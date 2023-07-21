;; smartparens-config.el
;;
;; Install smartparens package for use with elixir-mode
;;
;; Walter McGinnis, 2023-06-19
;;
;; http://github.com/walter/emakit

(use-package smartparens
  :diminish smartparens-mode
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

(provide 'smartparens-config)
