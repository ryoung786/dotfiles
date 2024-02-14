(use-package olivetti
  :delight
  :custom (olivetti-body-width 105))

(use-package markdown-mode
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.livemd\\'" . gfm-mode))
  :init
  (require 'org)
  :custom
  (markdown-fontify-code-blocks-natively t)
  :hook
  (markdown-mode . olivetti-mode)
  (markdown-mode . variable-pitch-mode)
  :custom-face
  (markdown-code-face ((t (:inherit 'fixed-pitch))))
  (markdown-header-face-1 ((t (:height 1.8 :inherit org-level-1))))
  (markdown-header-face-2 ((t (:height 1.6 :inherit org-level-2))))
  (markdown-header-face-3 ((t (:height 1.4 :inherit org-level-3))))
  (markdown-header-face-4 ((t (:height 1.2 :inherit org-level-4))))
  (markdown-header-face-5 ((t (:height 1.1 :inherit org-level-5)))))

;; There isn't an eldoc-mode hook, so we have to use special-mode instead.
(add-hook 'special-mode-hook
          (lambda ()
            (set-face-attribute 'markdown-code-face nil :inherit 'fixed-pitch)
            (set-face-attribute 'markdown-pre-face nil :inherit 'fixed-pitch)
            (if (string-match-p "\\*eldoc\\*" (buffer-name)) (variable-pitch-mode t))))

(provide 'markdown-config)
