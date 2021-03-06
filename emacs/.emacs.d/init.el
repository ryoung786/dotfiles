;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ensure use-package is installed, so we can install everything else

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents))

(dolist (package '(use-package))
   (unless (package-installed-p package)
       (package-install package)))

;; make sure we always download packages before we use and configure them
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; this needs to be early on for some reason :shrug:
(use-package exec-path-from-shell
  :init (when (memq window-system '(mac ns x))
          ;; (setq exec-path-from-shell-arguments '("-l"))
          (exec-path-from-shell-initialize)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Set a couple bare minimums


(fset 'yes-or-no-p 'y-or-n-p)
(setq initial-scratch-message ";; ╔═╗┌─┐┬─┐┌─┐┌┬┐┌─┐┬ ┬\n;; ╚═╗│  ├┬┘├─┤ │ │  ├─┤\n;; ╚═╝└─┘┴└─┴ ┴ ┴ └─┘┴ ┴\n")

(add-to-list 'default-frame-alist '(font . "Inconsolata-18"))

;; osx specific things
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super) ; make opt key do Super

(when (display-graphic-p)
  (menu-bar-mode 1))

(unless (display-graphic-p)
  (menu-bar-mode -1))



(use-package default-text-scale
  :init (default-text-scale-mode 1))

;; backup file de-clutter
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

(use-package hungry-delete
  :diminish
  :config (setq global-hungry-delete-mode t)
  :bind (("C-]" . hungry-delete-backward)
         ("C-\\" . hungry-delete-forward)))

(use-package diminish
  :config (diminish 'eldoc-mode))

(use-package vterm
  :commands vterm
  :hook (vterm-mode . (lambda () (setq term-prompt-regexp "^\\([0-9][0-9]:[0-9][0-9] \$ \\|iex([0-9]+)> \\)")))
  :bind (("M-C-9" . multi-vterm-dedicated-toggle))
  :config (setq vterm-max-scrollback 10000))
(use-package multi-vterm)

(use-package ws-butler :diminish)

(use-package lsp-origami
  :diminish
  :hook (lsp-after-open . lsp-origami-try-enable))

(use-package markdown-mode
  :mode ("\\.\\(live\\)?md\\'" . gfm-mode)
  :commands (markdown-mode gfm-mode)
  :bind ("C-c C-c l" . my-markdown-preview)
  :config
  (add-to-list 'markdown-code-lang-modes '("json" . js-mode))
  (setq markdown-command "pandoc -t html5"
        markdown-fontify-code-blocks-natively t
        markdown-css-paths '("~/.emacs.d/markdown.css")))
(use-package grip-mode
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Language configuration

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map)
  :init (setq projectile-switch-project-action #'projectile-dired))

(use-package ripgrep)

(use-package counsel)

(use-package ivy
  :diminish ivy-mode
  :init (ivy-mode 1) ; globally at startup
  :bind (("M-x" . counsel-M-x)
         ("C-h v" . counsel-describe-variable)
         ("C-h f" . counsel-describe-function))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 7)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder))))

(use-package ivy-rich
  :hook (ivy-mode . ivy-rich-mode)
  :custom (ivy-rich-path-style 'abbrev)
  :init (ivy-rich-mode 1))

(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))

(use-package all-the-icons-ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

(use-package orderless
  :custom (completion-styles '(orderless)))

(use-package which-key
  :diminish which-key-mode
  :init (which-key-mode)
  :config
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-idle-secondary-delay 1.0))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :diminish lsp-mode
  ;; :custom (setq lsp-enable-links nil)
  :hook ((go-mode . lsp)
         (ruby-mode . lsp)
         (java-mode . lsp)
         (elixir-mode . lsp)
         (scala-mode . lsp))
  :config
  (setq lsp-ui-doc-enable nil
        lsp-file-watch-threshold 10000)
  :init (add-to-list 'exec-path "~/dev/elixir-ls/release"))

(use-package lsp-ui
  :requires lsp-mode flycheck
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package company
  :diminish
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq lsp-completion-provider :capf))
(use-package company-box
  :diminish
  :hook (company-mode . company-box-mode))

(use-package flycheck :diminish)

(use-package projectile-rails
  :config (projectile-rails-global-mode)
  :bind-keymap ("C-c r" . projectile-rails-command-map))

(use-package web-mode
  :mode
  (("\\html\\.[hl]?eex$" . web-mode)
   ("\\.html$" . web-mode))
  :config
  (setq web-mode-engines-alist '(("elixir" . "\\.html\\.[lh]?.eex\\'"))
        web-mode-extra-auto-pairs '(("elixir" . (("{{ " . " }}"))))
        web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'yaml-mode-hook 'display-line-numbers-mode)

(use-package diff-hl
  :hook (prog-mode . diff-hl-mode))

(use-package elixir-mode
  :bind (("C-c C-n" . flycheck-next-error)
         ("C-c C-p" . flycheck-previous-error))
  :hook (elixir-mode . (lambda () (add-hook 'before-save-hook 'elixir-format nil t))))

(use-package direnv
 :config (direnv-mode)
 :custom (direnv-always-show-summary nil))

(use-package org
  :bind (("C-c j l" . insert-jira-link)))

(use-package mmm-mode
  :config
  (setq mmm-global-mode 'maybe
        mmm-parse-when-idle 't
        mmm-set-file-name-for-modes '(web-mode))
  (let ((class 'elixir-eex)
        (submode 'web-mode)
        (front "^[ ]+~[H|L]\"\"\"")
        (back "^[ ]+\"\"\""))
    (mmm-add-classes (list (list class :submode submode :front front :back back)))
    (mmm-add-mode-ext-class 'elixir-mode nil class)))

(define-advice web-mode-guess-engine-and-content-type (:around (f &rest r) guess-engine-by-extension)
  (if (and buffer-file-name (equal "ex" (file-name-extension buffer-file-name)))
      (progn (setq web-mode-content-type "html")
             (setq web-mode-engine "elixir")
             (web-mode-on-engine-setted))
    (apply f r)))

(use-package yasnippet
  :diminish
  :hook ((prog-mode . yas-minor-mode)
	 (conf-mode . yas-minor-mode)
	 (text-mode . yas-minor-mode)
	 (snippet-mode . yas-minor-mode)))

(use-package yasnippet-snippets
  :after (yasnippet))

(use-package rjsx-mode
  :mode
  (("\\.js" . rjsx-mode)
   ("\\.tsx" . rjsx-mode)))

(use-package yaml-mode)

(use-package prettier-js
  :hook ((js-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)
         (rjsx-mode . prettier-js-mode)))

(setq-default js-indent-level 2)
(setq-default css-indent-offset 2)
(setq-local web-mode-markup-indent-offset 2) ; web-mode, html tag in html file
(setq-local web-mode-css-indent-offset 2) ; web-mode, css in html file
(setq-local web-mode-code-indent-offset 2)


(use-package go-mode
  :hook (go-mode . (lambda ()
                     (setq tab-width 2)

                     (lsp-register-custom-settings
                      '(("gopls.completeUnimported" t t)
                        ("gopls.staticcheck" t t)))
                     ))
  :hook (go-mode . (lambda () (add-hook 'before-save-hook 'gofmt nil t)))
  )

(use-package lsp-java
  :hook (java-mode . (lambda ()
                       (setq tab-width 2)
                       (setq c-basic-offset 2)))
  :config
  (setq lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")
  (setq lsp-java-format-settings-profile "GoogleStyle"))
(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))

(setq ruby-insert-encoding-magic-comment nil)

(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")
(use-package sbt-mode
  :commands sbt-start sbt-command
  :custom (sbt:default-command "testQuick")
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))
(use-package lsp-metals)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom Functions

;; taken from emacs wiki
;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
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

;; indent/unindent region
(defun indent-rigid-by-4 (start end)
  "Indents the region 4 spaces to the right"
  (interactive "r")
  (indent-code-rigidly start end 4))
(defun unindent-rigid-by-4 (start end)
  "Indents the region 4 spaces to the right"
  (interactive "r")
  (indent-code-rigidly start end -4))

;; Taken from Steve Yegge's .emacs
;; someday might want to rotate windows if more than 2 of them
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

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

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

(defun align-whitespace (start end)
  "Align columns by whitespace"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 0 t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom key bindings

(global-set-key "\M-;" 'comment-dwim-line)
(global-set-key (kbd "C-o") 'swap-windows)
(global-set-key (kbd "C-a") 'beginning-of-line-dwim)
(global-set-key (kbd "C-e") 'end-of-line-dwim)
(global-set-key (kbd "<up>") 'scroll-down-one)
(global-set-key (kbd "<down>") 'scroll-up-one)
(global-set-key (kbd "C-M-<up>") 'scroll-other-window-down-one)
(global-set-key (kbd "C-M-<down>") 'scroll-other-window-up-one)
(global-set-key (kbd "C-x i") 'previous-multiframe-window)
(global-set-key (kbd "ESC <right>") 'indent-rigid-by-4)
(global-set-key (kbd "ESC <left>") 'unindent-rigid-by-4)

;; This doesn't work for emacs --daemon / emacsclient restarts :(
(set-terminal-coding-system 'utf-8)
(put 'upcase-region 'disabled nil)

(use-package doom-themes
  :config
  (load-theme 'doom-monokai-pro t))

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(column-number-mode t)
 '(compilation-message-face 'default)
 '(fci-rule-color "#3C3D37")
 '(highlight-changes-colors '("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors
   '(("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100)))
 '(ido-mode 'both nil (ido))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(lsp-file-watch-ignored-directories
   '("[/\\\\]\\.git$" "[/\\\\]\\.hg$" "[/\\\\]\\.bzr$" "[/\\\\]_darcs$" "[/\\\\]\\.svn$" "[/\\\\]_FOSSIL_$" "[/\\\\]\\.idea$" "[/\\\\]\\.ensime_cache$" "[/\\\\]\\.eunit$" "[/\\\\]node_modules$" "[/\\\\]\\.fslckout$" "[/\\\\]\\.tox$" "[/\\\\]\\.stack-work$" "[/\\\\]\\.bloop$" "[/\\\\]\\.metals$" "[/\\\\]target$" "[/\\\\]\\.ccls-cache$" "[/\\\\]\\.deps$" "[/\\\\]build-aux$" "[/\\\\]autom4te.cache$" "[/\\\\]\\.reference$" "[/\\\\]_build$" "[/\\\\]deps$"))
 '(lsp-ui-doc-mode nil t)
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   '(direnv dockerfile-mode all-the-icons-completion sbt-mode yaml-mode ein csv-mode orderless ripgrep olivetti scala-mode grip-mode all-the-icons-ivy all-the-icons-ivy-rich erlang lfe-mode yasnippet-snippets ws-butler which-key web-mode use-package tree-sitter-langs rjsx-mode projectile-rails prettier-js multi-vterm mmm-mode lsp-ui lsp-origami lsp-java ivy-rich hungry-delete go-mode flycheck exec-path-from-shell elixir-mode doom-themes diminish diff-hl default-text-scale counsel company-box))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF")))
 '(vc-annotate-very-old-color nil)
 '(warning-suppress-types '((comp)))
 '(weechat-color-list
   '(unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0"))
 '(ws-butler-global-mode t)
 '(yas-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-insert ((t (:background "DarkSeaGreen4" :foreground "DarkSeaGreen4"))))
 '(header-line ((t (:inherit nil :background "#000629"))))
 '(hl-line ((t (:extend t :background "gray5"))))
 '(isearch ((t (:inherit match :background "NavajoWhite3" :foreground "windowBackgroundColor" :box nil :underline t))))
 '(line-number ((t (:inherit default :foreground "gray40" :strike-through nil :underline nil :slant normal :weight normal))))
 '(line-number-current-line ((t (:inherit (hl-line default) :foreground "maroon1" :strike-through nil :underline nil :slant normal :weight normal))))
 '(linum ((t (:inherit default :foreground "gray40" :strike-through nil :underline nil :slant normal :weight normal))))
 '(lsp-face-highlight-textual ((t (:background "CadetBlue4" :foreground "#FCFCFA" :weight normal))))
 '(mode-line ((t (:background "dim gray" :foreground "gray90" :box (:line-width 3 :color "light coral")))))
 '(mode-line-inactive ((t (:background "dim gray" :foreground "gray90" :box nil))))
 '(shadow ((t (:foreground "gray40"))))
 '(tree-sitter-hl-face:function\.method\.call ((t)) t)
 '(tree-sitter-hl-face:method\.call ((t)))
 '(tree-sitter-hl-face:operator ((t)))
 '(tree-sitter-hl-face:property ((t)))
 '(tree-sitter-hl-face:variable ((t)))
 '(vterm-color-black ((t (:background "MediumPurple1" :foreground "#19181A"))))
 '(web-mode-variable-name-face ((t (:inherit font-lock-variable-name-face :foreground "plum")))))
