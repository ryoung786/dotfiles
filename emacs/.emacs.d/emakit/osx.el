;; osx.el
;;
;; Configure mac specific settings
;;
;; Ryan Young, 2023-07-05

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super) ; make opt key do Super
  (setq ns-pop-up-frames nil))

(use-package kkp
  :config
  (global-kkp-mode 1))

(provide 'osx)
