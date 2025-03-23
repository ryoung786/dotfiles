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

(setq use-package-always-ensure 't)

(use-package delight)

;; we depend on treesit-auto to load grammers on first use of a mode
;; https://github.com/renzmann/treesit-auto
(setq treesit-auto-install 't)
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
(require 'elixir-config)
(require 'flymake-config)
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
(require 'lsp-config)
(require 'appearance)
(require 'css-js-config)
(require 'sql-config)

(nerd-icons-completion-mode t)

(provide 'emakit)
