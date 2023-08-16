;; emakit
;; http://github.com/walter/emakit
;; set the dotfiles-dir variable to this directory
(setq kitfiles-dir (concat (file-name-directory
                    (or (buffer-file-name) load-file-name)) "/emakit"))

;; set up our various directories to load
(add-to-list 'load-path kitfiles-dir)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; backup file de-clutter
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )


(require 'emakit)
(put 'dired-find-alternate-file 'disabled nil)
