(add-to-list 'default-frame-alist '(font . "VictorMono Nerd Font-14"))

(set-face-attribute 'default nil :font "VictorMono Nerd Font" :weight 'light :height 140)
(set-face-attribute 'fixed-pitch nil :font "VictorMono Nerd Font" :weight 'light :height 140)
(set-face-attribute 'variable-pitch nil :font "IBM Plex Sans-16" :weight 'light)

(setq-default line-spacing .2)

(use-package default-text-scale
  :init (default-text-scale-mode 1))

;; Improved Spell Checker (depends on enchant system library)
(use-package jinx
  :delight
  :bind (("C-M-$" . jinx-correct))
  :config (global-jinx-mode nil))

(provide 'font)
