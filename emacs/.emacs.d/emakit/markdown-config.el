(use-package markdown-mode
  :ensure t
  :mode
  (
   ("README\\.md\\'" . gfm-mode)
   ("\\.livemd\\'" . gfm-mode)))

(provide 'markdown-config)
