(use-package vterm
  :commands vterm
  :hook (vterm-mode . (lambda () (setq term-prompt-regexp "^\\([0-9][0-9]:[0-9][0-9] \$ \\|iex([0-9]+)> \\)")))
  :config
  (setq vterm-max-scrollback 10000)
  (add-to-list 'display-buffer-alist
             '("\\*vterm\\*"
                (display-buffer-in-side-window)
                (window-height . 0.25)
                (slot . -1)
                (side . bottom)
                (window-parameters . ((no-delete-other-windows . t))))))
(use-package multi-vterm
  :bind (("M-C-8" . multi-vterm-project)))
(use-package vterm-toggle
  :bind (("M-C-9" . multi-vterm-dedicated-toggle)))

(provide 'vterm-config)
