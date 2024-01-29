(use-package olivetti
  :delight
  :custom olivetti-body-width 120)

(use-package markdown-mode
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.livemd\\'" . gfm-mode))
  :init
  (require 'org)
  :hook
  ((markdown-mode . olivetti-mode)
   (markdown-mode . variable-pitch-mode)
   (markdown-mode . (lambda () (set-face-attribute 'markdown-code-face nil :inherit 'fixed-pitch))))
  :custom-face
  (markdown-header-face-1 ((t (:height 1.8 :inherit org-level-1))))
  (markdown-header-face-2 ((t (:height 1.6 :inherit org-level-2))))
  (markdown-header-face-3 ((t (:height 1.4 :inherit org-level-3))))
  (markdown-header-face-4 ((t (:height 1.2 :inherit org-level-4))))
  (markdown-header-face-5 ((t (:height 1.1 :inherit org-level-5)))))

(provide 'markdown-config)
