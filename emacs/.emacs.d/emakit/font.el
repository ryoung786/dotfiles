(add-to-list 'default-frame-alist '(font . "Fira Code-14"))
(setq-default line-spacing .2)

(use-package default-text-scale
  :init (default-text-scale-mode 1))

;; Improved Spell Checker (depends on enchant system library)
(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind (("C-M-$" . jinx-correct)))

(provide 'font)
