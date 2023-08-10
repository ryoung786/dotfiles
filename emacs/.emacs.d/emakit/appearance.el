;; appearance.el
;;
;; Set theme, turn off annoyances, etc.
;;
;; Walter McGinnis, 2022-05-18
;;
;; http://github.com/walter/emakit

(use-package emacs
  :custom-face
  (ansi-color-black ((t (:background "dark orchid" :foreground "dark orchid"))))
  (ansi-color-bright-black ((t (:background "MediumPurple1" :foreground "MediumPurple1"))))
  (ansi-color-bright-magenta ((t (:background "orchid1" :foreground "orchid1"))))
  (ansi-color-magenta ((t (:background "magenta" :foreground "magenta"))))
  (blamer-face ((t :foreground "#7a88cf" :background nil :height 140 :italic t)))
  (fixed-pitch ((t (:family "Inconsolata"))))
  (font-lock-comment-face ((t (:foreground "bisque3" :slant italic :family "Fira Code iScript"))))
  (font-lock-doc-face ((t (:inherit font-lock-comment-face :foreground "bisque3"))))
  (flymake-error-echo ((t :foreground "LightCoral" :weight normal)))
  (header-line ((t (:inherit nil :background "DarkOrchid4"))))
  (hl-line ((t (:extend t :background "gray35"))))
  (isearch ((t (:background "hotpink" :foreground "white"))))
  (lazy-highlight ((t (:inherit match :background "LightCyan4"))))
  (line-number ((t (:inherit default :foreground "gray60" :slant italic :weight normal))))
  (line-number-current-line ((t (:inherit (hl-line default) :background "DarkSlateGray" :foreground "snow1"))))
  (markdown-blockquote-face ((t (:foreground "dark gray"))))
  (mode-line ((t (:background "DarkSlateGray" :foreground "snow1" :box (:line-width (1 . 1) :color "snow" :style flat-button)))))
  (mode-line-inactive ((t (:background "gray30" :foreground "#f2fffc" :box (:line-width (1 . 1) :color "gray30" :style released-button)))))
  (region ((t (:background "DarkSlateGray"))))
  (shadow ((t (:foreground "gray50"))))
  (vertico-current ((t (:background "DarkSlateGray"))))
  (vterm-color-black ((t (:background "MediumPurple1" :foreground "#19181A"))))
  (web-mode-variable-name-face ((t (:inherit font-lock-variable-name-face :foreground "plum"))))
  )

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)

(use-package autorevert :delight auto-revert-mode)
(use-package eldoc :delight eldoc-mode)

(setq help-window-select t)

(use-package doom-themes
  :config (load-theme 'doom-material t))

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))
    ))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'yaml-mode-hook 'display-line-numbers-mode)
(setq column-number-mode t)

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package highlight-indent-guides
  :delight
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-auto-enabled nil)
  (highlight-indent-guides-method 'character)
  :custom-face
  (highlight-indent-guides-character-face ((t (:foreground "gray40")))))

(setq flymake-mode-line-lighter "") ; Don't display "Flymake[0 0]", instead just "[0 0]"

(provide 'appearance)
