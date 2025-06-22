;; css-js-config.el

(setq-default js-indent-level 2)
(setq-default css-indent-offset 2)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-ts-mode))

(provide 'css-js-config)
