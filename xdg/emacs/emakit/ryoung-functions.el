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

(use-package hungry-delete
  :delight
  :config (setq global-hungry-delete-mode t)
  :bind (("C-]" . hungry-delete-backward)
         ("C-\\" . hungry-delete-forward)))


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
(global-set-key (kbd "C-x i") '(lambda () (interactive) (other-window -1)))
(global-set-key (kbd "ESC <right>") 'indent-rigid-by-4)
(global-set-key (kbd "ESC <left>") 'unindent-rigid-by-4)

(provide 'ryoung-functions)
