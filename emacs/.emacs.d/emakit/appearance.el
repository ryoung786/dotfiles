;; appearance.el
;;
;; Set theme, turn off annoyances, etc.
;;
;; Walter McGinnis, 2022-05-18
;;
;; http://github.com/walter/emakit

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-monokai-machine t))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 20
        doom-modeline-icon nil))

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))
    ))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'yaml-mode-hook 'display-line-numbers-mode)

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(provide 'appearance)
