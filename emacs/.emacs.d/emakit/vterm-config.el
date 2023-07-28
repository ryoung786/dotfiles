(defun ry/toggle-dedicated-vterm ()
  "Open a vterm or switch focus to it if it's already visible"
  (interactive)
  (if-let ((window (get-buffer-window "*vterm*")))
      (if (string= (buffer-name (current-buffer)) "*vterm*")
	  (ry/switch-to-mru-window)
	(select-window window))
    (vterm)))

(defun ry/toggle-project-vterm ()
  "Open a project's vterm or switch focusto it if it's already visible"
  (interactive)
  (if-let* ((buf-name (concat "*vterm " (projectile-project-name) "*"))
	    (window (get-buffer-window buf-name)))
      (if (string= (buffer-name (current-buffer)) buf-name)
	  (ry/switch-to-mru-window)
	(select-window window))
    (if (buffer-live-p (get-buffer buf-name))
	(pop-to-buffer buf-name)
      (projectile-run-vterm-other-window))))

(defun ry/switch-to-mru-window ()
  (interactive)
  (if-let ((mru-window (get-mru-window nil nil t)))
      (select-window mru-window)
    (quit-windows-on (window-buffer mru-window))))

(use-package vterm
  :commands vterm
  :hook (vterm-mode . (lambda () (setq term-prompt-regexp "^\\([0-9][0-9]:[0-9][0-9] \$ \\|iex([0-9]+)> \\)")))
  :bind (("C-M-8" . ry/toggle-project-vterm)
	 ("C-M-9" . ry/toggle-dedicated-vterm))
  :config
  (setq vterm-max-scrollback 10000)
  (add-to-list 'display-buffer-alist
             '("\\*vterm\\*"
                (display-buffer-in-side-window)
                (window-height . 0.25)
                (slot . -1)
                (side . bottom)
                (window-parameters . ((no-delete-other-windows . t))))))

(provide 'vterm-config)
