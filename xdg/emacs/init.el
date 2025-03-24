;; First time installation notes:
;; If on Mac Sequoia 15.3 or higher, you may have to install
;; some of the packages via package-install rather than use-package

(setq kitfiles-dir (concat (file-name-directory
                            (or (buffer-file-name) load-file-name)) "/emakit"))

;; set up our various directories to load
(add-to-list 'load-path kitfiles-dir)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; backup file de-clutter
(setq backup-directory-alist '(("." . "~/.config/emacs/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )
(setq auto-save-file-name-transforms '((".*" "~/.config/emacs/autosave/" t)))
(setq create-lockfiles nil)

(require 'emakit)
(put 'dired-find-alternate-file 'disabled nil)
