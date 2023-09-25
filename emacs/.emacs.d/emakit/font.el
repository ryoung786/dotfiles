(add-to-list 'default-frame-alist '(font . "VictorMono Nerd Font-14"))
(setq-default line-spacing .2)

(use-package default-text-scale
  :init (default-text-scale-mode 1))

;; Improved Spell Checker (depends on enchant system library)
(use-package jinx
  :delight
  :bind (("C-M-$" . jinx-correct))
  :config (global-jinx-mode nil))

(provide 'font)
