;; -*- lexical-binding: t; -*-

;;;; General Emacs behaviour

;;; Packages

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(setq use-package-always-ensure 't)


(use-package delight)
(use-package autorevert :delight auto-revert-mode)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq column-number-mode t)
(put 'dired-find-alternate-file 'disabled nil)
(use-package nerd-icons-dired :hook dired-mode)


;;; Clean up littering
;; Emacs by default leaves random files in places like the home directory or the top level of the config
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(setq backup-directory-alist '(("." . "~/.config/emacs/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

(setq auto-save-file-name-transforms '((".*" "~/.config/emacs/autosave/" t)))

(setq create-lockfiles nil)

;;;; Programming config

(use-package flymake
  :custom
  (flymake-mode-line-lighter "")

  :bind
  (:map flymake-mode-map
        ("C-c C-n" . flymake-goto-next-error)
	("C-c C-p" . flymake-goto-prev-error)))

;;; Org mode config

(use-package org
  :ensure nil
  :hook
  (org-mode . variable-pitch-mode)
  (org-mode . visual-line-mode)

  :custom
  (org-hide-emphasis-markers t)

  :custom-face
  (org-document-title ((t (:height 2.0 :weight light :family "Kalam"))))
  (org-level-1 ((t (:height 1.2))))
  (org-level-2 ((t (:height 1.2))))
  (org-level-3 ((t (:height 1.2))))
  (org-level-4 ((t (:height 1.2))))
  (org-level-5 ((t (:height 1.2))))

  :init
  (require 'org)
  (dolist (item '(("sh" . "src sh")
                  ("bsh" . "src bash")
                  ("em" . "src emacs-lisp")
                  ("li" . "src lisp")
                  ("el" . "src elixir")
                  ("ts" . "src typescript")
                  ("py" . "src python")
                  ("yaml" . "src yaml")
                  ("json" . "src json")))
    (add-to-list 'org-structure-template-alist item)))

(use-package org-modern
  :after org
  :config (global-org-modern-mode)
  :custom (org-modern-star 'replace))

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autosubmarkers t
        org-appear-autoentities t
        org-appear-autolinks t
        org-appear-inside-latex t))

(use-package org
  :ensure nil
  :config
  (make-directory "~/org/notes/" :parents)

  (setq org-capture-templates
        '(("n" "Note" entry (file+olp+datetree "~/org/notes/daily.org")
           "* %U %^{Title} %^g\n%?\n\n%i"
           :empty-lines 1)))

  :bind ("C-c c" . org-capture))

(use-package org-agenda
  :ensure nil
  :no-require t
  :custom (org-agenda-files '("~/org/notes/"))
  :bind ("C-c a" . org-agenda))

(use-package markdown-ts-mode
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.livemd\\'" . gfm-mode))
  :custom
  (markdown-fontify-code-blocks-natively t)
  :hook
  (markdown-mode . olivetti-mode)
  (markdown-mode . variable-pitch-mode))

(use-package eldoc
  :delight eldoc-mode
  :after markdown-ts-mode
  :custom (help-window-select t)
  :hook
  (eldoc-mode . (lambda ()
                  (set-face-attribute 'markdown-code-face nil :inherit 'fixed-pitch)
                  (set-face-attribute 'markdown-pre-face nil :inherit 'fixed-pitch)
                  (if (string-match-p "\\*eldoc\\*" (buffer-name)) (variable-pitch-mode t)))))

(setq help-window-select t)

(use-package olivetti
  :delight
  :custom (olivetti-body-width 105))

(use-package prog-mode
  :ensure nil
  :hook
  (prog-mode . display-line-numbers-mode)
  (prog-mode . (lambda () (indent-tabs-mode -1))))

;;; Auto formatting code files: Apheleia

(use-package apheleia
  :delight apheleia-mode
  :init (apheleia-global-mode +1)
  :config
  (add-to-list 'apheleia-mode-alist '(heex-ts-mode . mix-format))
  (dolist (mode '(js-ts-mode jsx-ts-mode typescript-ts-mode tsx-ts-mode css-ts-mode css-mode graphql-mode html-mode html-ts-mode js3-mode json-mode json-ts-mode js-json-mode js-mode js-ts-mode scss-mode typescript-mode web-mode yaml-mode yaml-ts-mode markdown-ts-mode markdown-mode toml-ts-mode))
    (setf (alist-get mode apheleia-mode-alist) 'oxfmt)))

(use-package yasnippet
  :delight yas-minor-mode
  :hook ((prog-mode . yas-minor-mode))
  :config
  (setq yas-snippet-dirs '("~/.config/emacs/private-snippets" "~/.config/emacs/snippets"))
  (yas-reload-all))

(use-package yasnippet-snippets :after yasnippet)

(use-package yaml-mode
  :hook (yaml-mode . display-line-numbers-mode))

;;; Elixir

(defun ry/iex-send-string (str)
  "Sends the str to the project's IEx session and executes it."
  (let ((window-config (current-window-configuration)))
    (ry/toggle-project-vterm)
    (with-current-buffer (window-buffer)
      (vterm-reset-cursor-point)
      (let* ((inhibit-read-only t)
             (line-num (line-number-at-pos))
             (last-line (save-excursion
                          (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position)))))
        (if (string-match "^iex" last-line)
            (progn
              (vterm-send-string str t)
              (vterm-send-return))
          (progn
            (message (concat "No IEx session running: " last-line))
            (set-window-configuration window-config)))))))

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
      (ry/iex-send-string (format "r %s" mod)))))

(defun ry/iex-send-current-line-or-region ()
  "Insert text of current line or region in IEx and execute."
  (interactive)
  (let* ((current-line (buffer-substring
                        (save-excursion
                          (beginning-of-line)
                          (point))
                        (save-excursion
                          (end-of-line)
                          (point))))
         (buf (current-buffer))
         (command (string-trim
                   (if (use-region-p)
                       (buffer-substring (region-beginning) (region-end))
                     current-line))))
    (ry/iex-send-string command)))

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

;;; HTML, JS, CSS

(progn
  (setq-default js-indent-level 2)
  (setq-default css-indent-offset 2)

  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-ts-mode)))

(use-package web-mode)

;; web-mode specific overrides of tab settings
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-indent-style 2))

(add-hook 'web-mode-hook  'web-mode-hook)

(add-to-list 'auto-mode-alist '("\\.html.eex\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.handlebars\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(use-package sql
  :ensure nil
  :no-require t
  :hook (sql-mode . (lambda () (setq tab-width 2))))

;;; Eglot LSP

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(sql-mode . ("sqls")))

  (add-to-list 'eglot-server-programs
               `((elixir-ts-mode heex-ts-mode) .
                 ,(eglot-alternatives '(("dexter" "lsp")
                                        ("expert" "--stdio")))))
  :hook
  (typescript-ts-mode . eglot-ensure)
  (tsx-ts-mode . eglot-ensure)
  (js-ts-mode . eglot-ensure)
  (sql-mode . eglot-ensure)
  (elixir-ts-mode . eglot-ensure)
  (heex-ts-mode . eglot-ensure))

;;; Shells and environment variable config

(setenv "ESHELL" "/bin/zsh")
(setenv "SHELL" "/bin/zsh")

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (setq exec-path-from-shell-variables
        '("PATH" "MANPATH" "EDITOR" "GPG_TTY"
          "XDG_CONFIG_HOME" "XDG_DATA_HOME" "XDG_CACHE_HOME" "ZDOTDIR"
          "AWS_CA_BUNDLE" "CURL_CA_BUNDLE" "REQUESTS_CA_BUNDLE"
          "SSL_CERT_FILE" "NODE_EXTRA_CA_CERTS" "HEX_CACERTS_PATH" "GIT_SSL_CAINFO"))
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

(use-package mise
  :delight mise-mode
  :init (add-hook 'after-init-hook #'global-mise-mode))

(defun ry/toggle-dedicated-vterm ()
  "Open a vterm or switch focus to it if it's already visible"
  (interactive)
  (if-let* ((window (get-buffer-window "*vterm*")))
      (if (string= (buffer-name (current-buffer)) "*vterm*")
	  (ry/switch-to-mru-window)
	(select-window window))
    (vterm)))

;; A project vterm's buffer is *vterm <project-name.*
(defun ry/toggle-project-vterm ()
  "Open a project's vterm or switch focus to it if it's already visible"
  (interactive)
  (if-let* ((buf-name (concat "*vterm " (project-name (project-current)) "*"))
	    (window (get-buffer-window buf-name)))
      (if (string= (buffer-name (current-buffer)) buf-name)
	  (ry/switch-to-mru-window)
	(select-window window))
    (if (buffer-live-p (get-buffer buf-name))
	(pop-to-buffer buf-name)
      (projectile-run-vterm-other-window))))

;; mru = Most Recently Used
(defun ry/switch-to-mru-window ()
  (interactive)
  (if-let* ((mru-window (get-mru-window nil nil t)))
      (select-window mru-window)
    (quit-windows-on (window-buffer mru-window))))

(use-package vterm
  :commands vterm
  :hook
  (vterm-mode . (lambda () (setq term-prompt-regexp "^\\([0-9][0-9]:[0-9][0-9] \$ \\|iex([0-9]+)> \\)")))
  :bind
  (("C-M-8" . ry/toggle-project-vterm)
   ("C-M-9" . ry/toggle-dedicated-vterm))
  :config
  (setq vterm-max-scrollback 10000)
  ;; Position the dedicated vterm buffer to be at the bottom
  (add-to-list 'display-buffer-alist
               '("\\*vterm\\*"
                 (display-buffer-in-side-window)
                 (window-height . 0.25)
                 (slot . -1)
                 (side . bottom)
                 (window-parameters . ((no-delete-other-windows . t)))))
  (if (display-graphic-p)
      (set-fontset-font nil 'symbol (font-spec :script 'symbol))))

(use-package kkp :config (global-kkp-mode 1))

(use-package magit
  :ensure t
  :pin melpa
  :config (global-set-key (kbd "C-x g") 'magit-status)
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

(use-package which-key
  :delight which-key-mode
  :config (which-key-mode))

;; spell checker
(use-package jinx
  :delight
  :bind (("C-M-$" . jinx-correct))
  :config (global-jinx-mode nil))

;;; Whitespace and Comments

(require 'whitespace)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq whitespace-action '(auto-cleanup)) ;; automatically clean up bad whitespace
(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab)) ;; only show bad whitespace

(defun align-whitespace (start end)
  "Align columns by whitespace"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 0 t))

(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
   If no region is selected and current line is not blank and we are not at
   the end of the line, then comment current line.
   Replaces default behaviour of comment-dwim, when it inserts comment at
   the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (region-active-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (goto-char start)
        (let ((real-start (line-beginning-position)))
          (goto-char end)
          (comment-or-uncomment-region real-start (line-end-position))))
    (comment-or-uncomment-region (line-beginning-position)
				 (line-end-position))))

(global-set-key "\M-;" 'comment-dwim-line)

(use-package ry/swap-windows
  :ensure nil
  :no-require t
  :init
  (defun swap-windows ()
    "If you have 2 windows, it swaps them."
    (interactive)
    (cond ((not (= (count-windows) 2))
  	   (message "You need exactly 2 windows to do this."))
          (t
           (let* ((w1 (nth 1 (window-list)))
                  (w2 (nth 2 (window-list)))
                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))
                  (s1 (window-start w1))
                  (s2 (window-start w2)))
             (set-window-buffer w1 b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)))))

  (defun other-window-backwards ()
    (interactive)
    (other-window -1))

  :bind
  (("C-o" . swap-windows)
   ("C-x i" . other-window-backwards)))

(use-package ry/line-dwim
  :ensure nil
  :no-require t
  :init
  (defun beginning-of-line-dwim ()
    (interactive)
    (if (eq (current-column) 0)
        (back-to-indentation)
      (beginning-of-line)))

  (defun end-of-line-dwim ()
    (interactive)
    (if (not (eq (point) (line-end-position)))
        (end-of-line)
      (progn
        (search-backward-regexp "[^\t ]")
        (forward-char))))
  :bind (("C-a" . beginning-of-line-dwim)
         ("C-e" . end-of-line-dwim)))

(use-package ry/scrolling
  :ensure nil
  :no-require t
  :init
  (pixel-scroll-precision-mode)

  (defun scroll-up-one ()
    (interactive)
    (scroll-up 1))

  (defun scroll-down-one ()
    (interactive)
    (scroll-up -1))

  (defun scroll-other-window-up-one ()
    (interactive)
    (scroll-other-window 1))

  (defun scroll-other-window-down-one ()
    (interactive)
    (scroll-other-window-down 1))

  (defun scroll-right-one ()
    (interactive)
    (scroll-right 1))

  (defun scroll-left-one ()
    (interactive)
    (scroll-left 1))
  :bind (("<up>" . scroll-down-one)
         ("<down>" . scroll-up-one)
         ("C-M-<up>" . scroll-other-window-down-one)
         ("C-M-<down>" . scroll-other-window-up-one)))

(use-package hungry-delete
  :delight
  :config (setq global-hungry-delete-mode t)
  :bind (("C-]" . hungry-delete-backward)
         ("C-\\" . hungry-delete-forward)))

(global-set-key (kbd "M-o") #'browse-url)

;;;; Appearance: themes and fonts

(require-theme 'modus-themes)
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs nil
      modus-themes-mixed-fonts t
      modus-themes-variable-pitch-ui t
      modus-themes-disable-other-themes t)

(modus-themes-load-theme 'modus-operandi-tinted)

(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'modus-operandi-tinted t))
    ('dark (load-theme 'modus-vivendi t))))

(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

(setq my-font-s "IosevkaTerm Nerd Font Mono")
(add-to-list 'default-frame-alist '(font . "IosevkaTerm Nerd Font Mono-16"))

(set-face-attribute 'default nil :font my-font-s :weight 'light :height 160)
(set-face-attribute 'fixed-pitch nil :font my-font-s :weight 'light :height 160)
(set-face-attribute 'variable-pitch nil :font "Iosevka Etoile" :weight 'light :height 160)

(setq-default line-spacing .2)

(use-package default-text-scale
  :init (default-text-scale-mode 1))

(require 'ansi-color)
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter) ; colorize mix compile output

;; Only needed for the vterm dedicated window
(use-package projectile
  :delight projectile-mode
  :commands (projectile-project-name)
  :bind-keymap ("C-c p" . projectile-command-map)
  :init (setq projectile-switch-project-action #'projectile-dired))

(use-package project
  :custom
  (project-prompter 'project-prompt-project-name)
  :config
  (add-to-list 'project-switch-commands
               '(magit-project-status "Magit") 'append)
  (add-to-list 'project-switch-commands
               '(ry/toggle-project-vterm "VTerm") 'append)
  :bind
  (:map project-prefix-map
        ("m" . magit-project-status)
        ("v" . ry/toggle-project-vterm)))



;;;; Completion, vertico etc

(setq completion-cycle-threshold 3) ; TAB cycle if there are only few candidates

(use-package vertico
  :init
  (vertico-mode)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package savehist :init (savehist-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :custom (marginalia-align 'right)
  :init (marginalia-mode t)
  )

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package corfu
  :custom
  (corfu-auto t)  ;; Enable auto completion
  :bind
  (:map corfu-map ("s-SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode)
  (corfu-history-mode t)
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package corfu-popupinfo
  :ensure nil ; this module is an extension within corfu, not its own package
  :after corfu
  :hook corfu-mode
  :custom (corfu-popupinfo-delay 0)
  :config (corfu-popupinfo-mode))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :after (corfu eglot)
  :config
  ;; If eglot has no suggestions, then by default it stops.  Instead
  ;; I want it to continue through the `completion-at-point-functions` until
  ;; it finds matches
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  ;; No need for ispell completions in html-mode, but leave them in for text-mode
  :hook
  (html-mode . (lambda () (delete
                           'ispell-completion-at-point
                           completion-at-point-functions))))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi))

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"
  )

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package agent-shell
  :ensure t
  :bind (("C-M-7" . agent-shell)
         :map agent-shell-mode-map
         ("RET" . newline)
         ("C-c C-c" . shell-maker-submit)
         ("C-c C-k" . agent-shell-interrupt)))

(load (expand-file-name "private.el" user-emacs-directory) 'noerror)
