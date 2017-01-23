;;; CUSTOM CONFIGURATION OF PACKAGES ;;;
; enable the tomorrow color theme
(color-theme-tomorrow-night)
;(color-theme-leuven)
; show column numbers by default
(column-number-mode 1)
; enable line numbering
(global-linum-mode 1)
; match parenthesis command
(global-set-key "%" 'match-paren)
; setup window resizing
(global-set-key (kbd "C-x _") 'shrink-window)
(global-set-key (kbd "C-x ^") 'enlarge-window)
; set the default font to terminus
;(set-default-font "Terminus-14")
; do the same for all new frames
(add-to-list 'default-frame-alist 
      '(font . "Terminus-14"))

(defun match-paren (arg)
   "Go to the matching paren if on a paren; otherwise insert %."
   (interactive "p")
   (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
         ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
         (t (self-insert-command (or arg 1)))))

; configure window number
(require 'window-number)
(eval-after-load "window-number"
  '(progn
     (window-number-mode)
     (window-number-meta-mode)
     (winner-mode 1)))

(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

; use pdflatex
(setq latex-run-command "pdflatex")
(setq TeX-PDF-mode t)

; org mode specifics
(define-key global-map (kbd "<f9>") 'org-capture)
(custom-set-variables
 '(org-agenda-files (quote ("~/org/todo.org")))
 '(org-default-notes-file "~/org/notes.org")
 '(org-agenda-ndays 7)
 '(org-deadline-warning-days 14)
 '(org-agenda-window-setup 'current-window) ; do not destroy my nice window setup!
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-reverse-note-order t)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-log-done 'note)
 '(org-log-done 'time)
 '(org-todo-keywords
   '((sequence "TODO(t)" "STARTED(s@/!)" "WAITING(w@/!)" "DELEGATED(e@/!)" "APPT(@!)" "|" "DONE(d!)" "DEFERRED" "CANCELLED(c@)")))
 '(org-agenda-custom-commands
   (quote (("d" todo "DELEGATED" nil)
       ("c" todo "DONE|DEFERRED|CANCELLED" nil)
       ("w" todo "WAITING" nil)
       ("W" agenda "" ((org-agenda-ndays 21)))
       ("A" agenda ""
        ((org-agenda-skip-function
          (lambda nil
        (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
         (org-agenda-ndays 1)
         (org-agenda-overriding-header "Today's Priority #A tasks: ")))
       ("u" alltodo ""
        ((org-agenda-skip-function
          (lambda nil
        (org-agenda-skip-entry-if (quote scheduled) (quote deadline)
                      (quote regexp) "\n]+>")))
         (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
 '(org-capture-templates
   (quote (("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks") 
	    "* TODO %?\n  %u")
	   ("a" "Appointment" entry (file+headline "~/org/todo.org" "Appointments") 
	    "* APPT %?\n  Added: %u")
	   ("n" "Note" entry (file+datetree "~/org/note.org")
	    "* %? \n Entered on %U \n %a")))))

; setup python mode
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
; get the interesting bits from emacs-for-python mode

;(require 'epy-setup) ;; setup other loads, it is required!
;(require 'epy-python) ;; python facilities
;(require 'epy-completion) ;; give me the autocompletion stuff
;(require 'epy-editing) ;; some nifty editing tricks and bindings
;(require 'epy-bindings)

; set the c style

; default to c++ for .h and .cc files
(add-to-list 'auto-mode-alist '("\\.cc$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
;function to implement Indentation style
;Documentation/CodingStyle (kernel)
(defun linux-c-indent ()
  "adjusted defaults for C/C++ mode use with the Linux kernel."
  (interactive)
  (setq tab-width 4)
  ;;force spaces, to work with dumber editors
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4))

(add-hook 'c-mode-hook 'linux-c-indent)
(add-hook 'c-mode-hook (lambda() (c-set-style "K&R")))
(add-hook 'c++-mode-hook 'linux-c-indent)

;(defun my-c-mode-hook ()
;  (setq c-basic-offset 4
;        c-indent-level 4
;        c-default-style "linux"))
;(add-hook 'c-mode-common-hook 'my-c-mode-hook)

; load ros package
;; Load the library and start it up
;(require 'rosemacs)
;(invoke-rosemacs)
;; Optional but highly recommended: add a prefix for quick access
;; to the rosemacs commands
;(global-set-key "\C-x\C-r" ros-keymap)


;; add melpa repository as it contains way more packages!
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; setup special modes

; setup lua-mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(defvar lua-default-application "/home/k-gee/local/bin/th")
;(defvar lua-default-application "/home/k-gee/local/bin/torch")
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
(setq lua-mode-map (make-sparse-keymap))
(define-key lua-mode-map (kbd "C-c C-l") 'lua-send-current-line)
(define-key lua-mode-map (kbd "C-c C-r") 'lua-send-region)
;; make sure .m files are interpreted as matlab
;(add-to-list 'auto-mode-alist '("\\.m\\'" . matlab-mode))
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
;; also interpret cuda as c
;; make sure .m files are interpreted as matlab
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.cuh\\'" . c-mode))
; setup julia mode
(autoload 'julia-mode "julia-mode" "Julia editing mode." t)
(add-to-list 'auto-mode-alist '("\\.jl$" . julia-mode))

; setup hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-all-abbrevs
        try-expand-dabbrev
        try-expand-dabbrev-from-kill
        try-expand-dabbrev-all-buffers
        try-complete-file-name-partially
        try-complete-file-name
        ;;         try-expand-list
        ;;         try-expand-line
        ;;        try-complete-lisp-symbol-partially
        ;;        try-complete-lisp-symbol
        ))
; use it instead of standard dabbrev
(global-set-key "\M-/" 'hippie-expand)  


; setup printing
 (setq lpr-command "gtklp")


(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 8)))
