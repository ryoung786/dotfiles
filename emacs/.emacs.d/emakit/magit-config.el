;; magit-config.el
;;
;; load package, global key mapping, etc.
;;
;; Walter McGinnis, 2022-05-18
;;
;; http://github.com/walter/emakit

(use-package magit
  :ensure t
  :pin melpa
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  :bind ("s-i" . magit-blame))

(use-package git-gutter
  :delight
  :hook prog-mode
  :bind ("C-c s" . git-gutter:stage-hunk)
  :config (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package browse-at-remote
  :bind ("C-c g g" . 'browse-at-remote))

(use-package diff-mode
  :custom (diff-font-lock-syntax nil))

(provide 'magit-config)
