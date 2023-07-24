;; emakit.el
;;
;; Trigger loading of emakit configuration and package loading
;;
;; Walter McGinnis, 2023-06-19
;;
;; http://github.com/walter/emfakit

;;;;; IMPORTANT: this requires loading in ~/emacs.d/init.el
;;;;; or similar emacs start up file - see Install in README

;; Set up package stuff
;; from https://lucidmanager.org/productivity/configure-emacs/
;;
;; Define and initialise package repositories
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure 't)

(use-package delight)

;; we depend on treesit-auto to load grammers on first use of a mode
;; https://github.com/renzmann/treesit-auto
;;   (setq treesit-auto-install 't)
(use-package treesit-auto
  :demand t
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

;; Additional packages and configuration

(require 'adjust-shell)
(require 'magit-config)
(require 'yasnippet-config)
(require 'smartparens-config)
(require 'which-key-config)
(require 'load-env-vars-config)
(require 'exunit-config)
(require 'elixir-ts-mode-config)
(require 'eglot-config)
(require 'flymake-config)
(require 'mix-config)
(require 'web-config)
(require 'whitespace-config)
(require 'file-mode-mappings)
(require 'org-mode-config)
(require 'useful-functions)
(require 'more-packages)

;; my packages
(require 'osx)
(require 'font)
(require 'vterm-config)
(require 'projectile-config)
(require 'ryoung-functions)
(require 'vertico-marginalia-corfu-config)
(require 'consult-embark-config)
(require 'markdown-config)
(require 'imenu-config)
(require 'appearance)

(nerd-icons-completion-mode t)

(provide 'emakit)
