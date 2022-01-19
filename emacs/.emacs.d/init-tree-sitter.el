(defface tree-sitter-hl-face:warning
  '((default :inherit font-lock-warning-face))
  "Face for parser errors"
  :group 'tree-sitter-hl-faces)

(defun hook/tree-sitter-common ()
  (unless font-lock-defaults
    (setq font-lock-defaults '(nil)))
  (setq tree-sitter-hl-use-font-lock-keywords nil)
  (tree-sitter-mode +1)
  (tree-sitter-hl-mode +1))

(defun hook/elixir-tree-sitter ()
  (setq
   tree-sitter-hl-default-patterns
   (read
    (concat
     "["
     (s-replace "#match?" ".match?"
                (f-read-text (expand-file-name "~/dev/tree-sitter-elixir/queries/highlights.scm")))
     "]")))
  (hook/tree-sitter-common))

(use-package tree-sitter
  :diminish
  :hook ((elixir-mode . hook/elixir-tree-sitter))
  :custom-face
  (tree-sitter-hl-face:operator ((t)))
  (tree-sitter-hl-face:variable ((t)))
  (tree-sitter-hl-face:function.method.call ((t)))
  (tree-sitter-hl-face:property ((t)))
  :config
  (setq tree-sitter-debug-highlight-jump-region t)
  (setq tree-sitter-debug-jump-buttons t))

(use-package tree-sitter-langs
  :after tree-sitter
  )

(provide 'init-tree-sitter)
