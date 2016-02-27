
;; get the thing-cmds keybindings
(thgcmd-bind-keys)

(defun mark-a-word-or-thing (arg)
  "Select word on or before current point, and move point to beginning of word.
    
    With a prefix ARG, first prompts for type of things and select ARG things
    but you need to move the point to the beginnig of thing first.
    
    But if a thing has been selected, then extend the selection by one thing
    on the other side of the point.
    (So to select backwards, select to the right first.)"
  (interactive "P")
  (if (or arg mark-active)
      (call-interactively 'mark-thing)
    (skip-syntax-backward "w_")
    (mark-thing 'word)))

(global-set-key (kbd "M-@") 'mark-a-word-or-thing)

(global-set-key (kbd "C-M-h") 'backward-kill-word)

(global-set-key (kbd "C-x \\") 'align-regexp)

(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key [f1] 'menu-bar-mode)

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x C-p") 'find-file-at-point)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)

(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two

(global-set-key (kbd "C-x ^") 'join-line)

(global-set-key (kbd "C-x C-m") 'execute-extended-command)

(global-set-key (kbd "C-h a") 'apropos)

(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp
                 isearch-string
               (regexp-quote isearch-string))))))

(define-key global-map "\C-ca" 'org-agenda)

(define-key global-map "\C-cl" 'org-store-link)

(define-key global-map "\C-x\C-r" 'rgrep)

;; multiple-cursors stuff
(define-key ctl-x-map "\C-m" #'mc/mark-all-dwim)
; rebind mule-keymap away from C-X C-m
(define-key ctl-x-map (kbd "<return>") mule-keymap)

;;(global-set-key (kbd "C-c m c") 'mc/edit-lines)

;; marking/unmarking next
(global-set-key (kbd "C-c /") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c .") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c '") 'mc/unmark-next-like-this)
(global-set-key (kbd "C-c ;") 'mc/unmark-previous-like-this)

;; the rest of the mc bindings go in their own keymap
(define-prefix-command 'stokasto/mc-map)
;; C-x m is usually `compose-mail'. Bind it to something
;; else if you use this command.
(define-key ctl-x-map "m" 'stokasto/mc-map)
(define-key stokasto/mc-map "i" #'mc/insert-numbers)
(define-key stokasto/mc-map "h" #'mc-hide-unmatched-lines-mode)
(define-key stokasto/mc-map "a" #'mc/mark-all-like-this)
;;; Occasionally useful
(define-key stokasto/mc-map "d"
  #'mc/mark-all-symbols-like-this-in-defun)
(define-key stokasto/mc-map "r" #'mc/reverse-regions)
(define-key stokasto/mc-map "s" #'mc/sort-regions)
(define-key stokasto/mc-map "l" #'mc/edit-lines)
(define-key stokasto/mc-map "\C-a"
  #'mc/edit-beginnings-of-lines)
(define-key stokasto/mc-map "\C-e"
    #'mc/edit-ends-of-lines)
