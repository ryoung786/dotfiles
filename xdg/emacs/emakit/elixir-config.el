(require 'treesit)

(use-package elixir-mode)

(defun ry/elixir-module-at-point ()
  "Return the full nested Elixir module name at point by checking enclosing `defmodule` blocks."
  (interactive)
  (save-excursion
    (let ((modules '())
          (point-pos (point)))
      (goto-char (point-min))
      (while (re-search-forward
              "^\\s-*defmodule\\s-+\\([A-Z][A-Za-z0-9_\\.]*\\)\\s-+do\\b" nil t)
        (let ((module-name (substring-no-properties (match-string 1)))
              (start (match-beginning 0)))
          (condition-case nil
              (let ((end (save-excursion
                           (goto-char start)
                           (forward-sexp) ; move past the module's `do ... end`
                           (point))))
                (when (and (>= point-pos start)
                           (<= point-pos end))
                  (push module-name modules)))
            (error nil)))) ; skip malformed blocks safely
      (let ((full-name (string-join (reverse modules) ".")))
        (if (called-interactively-p 'interactive)
            (message "%s" full-name)
          (unless (string-empty-p full-name)
            full-name))))))

(defun ry/iex-reload-module-at-point ()
  "Reload the Elixir module at point in an IEx session using vterm.
If no IEx session is detected, restore the previous window configuration."
  (interactive)
  (let ((mod (ry/elixir-module-at-point)))
    (if (not mod)
        (message "No Elixir module found at point.")
      (let ((window-config (current-window-configuration)))
        (ry/toggle-project-vterm)
        (with-current-buffer (window-buffer)
          (vterm--goto-line -1)
          (let* ((inhibit-read-only t)
                 (last-line (save-excursion
                              (buffer-substring-no-properties (point) (line-end-position)))))
            (if (string-match "^iex" last-line)
                (progn
                  (vterm-send-string (format "r %s" mod) t)
                  (vterm-send-return))
              (progn
                (message "No IEx session running")
                (set-window-configuration window-config)))))))))

(use-package elixir-ts-mode
  :hook
  (elixir-ts-mode . mix-minor-mode)
  (elixir-ts-mode . exunit-mode)
  :config (global-subword-mode t)
  :delight subword-mode)

(add-to-list 'major-mode-remap-alist '(elixir-mode . elixir-ts-mode))

(use-package heex-ts-mode
  :hook
  (heex-ts-mode . display-line-numbers-mode)
  (heex-ts-mode . git-gutter-mode)
  (heex-ts-mode . (lambda () (indent-tabs-mode -1)))
  :init (add-to-list 'auto-mode-alist '("\\.[hl]?eex\\'" . heex-ts-mode)))

(use-package mix
  :delight mix-minor-mode
  :after elixir-ts-mode
  :config (add-hook 'elixir-ts-mode-hook 'mix-minor-mode))

(setq compilation-scroll-output t)

(use-package exunit
  :delight exunit-mode
  :after elixir-ts-mode
  :config
  ;; overwrite exunit's definition to prefer running tests from umbrella root
  (defun exunit-test-for-file (file)
    "Return the test file for FILE."
    (replace-regexp-in-string "^\\(apps/.*/\\)?lib/\\(.*\\)\.ex$" "\\1test/\\2_test.exs" file))

  (defun exunit-file-for-test (test-file)
    "Return the file which is tested by TEST-FILE."
    (replace-regexp-in-string "^\\(apps/.*/\\)?test/\\(.*\\)_test\.exs$" "\\1lib/\\2.ex" test-file))

  (defun exunit-project-root ()
    "Return the current project root.

This value is cached in a buffer local to avoid filesytem access
on every call."
    (or
     exunit-project-root
     (let ((root (or (locate-dominating-file default-directory "apps") (locate-dominating-file default-directory "mix.exs"))))
       (unless root
         (error "Couldn't locate project root folder.  Make sure the current file is inside a project"))
       (setq exunit-project-root (expand-file-name root)))))
  :bind
  (:map elixir-ts-mode-map
        ("C-c , a" . exunit-verify-all)
        ("C-c , A" . exunit-verify-all-in-umbrella)
        ("C-c , s" . exunit-verify-single)
        ("C-c , v" . exunit-verify)
        ("C-c , r" . exunit-rerun)
        ("C-c , t" . exunit-toggle-file-and-test)
        ("s-r" . exunit-rerun)
        ))

(provide 'elixir-config)
