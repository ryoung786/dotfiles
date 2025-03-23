(setq my-font-s "IosevkaTerm Nerd Font Mono")
(add-to-list 'default-frame-alist '(font . "IosevkaTerm Nerd Font Mono-16"))

(set-face-attribute 'default nil :font my-font-s :weight 'light :height 160)
(set-face-attribute 'fixed-pitch nil :font my-font-s :weight 'light :height 160)
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
