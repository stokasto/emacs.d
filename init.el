; first off get the nice el-get package manager
; as it makes everything much easier
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(add-to-list 'load-path "~/.emacs.d/local")

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

; create a list of packages that we want from el-get
(setq my-el-get-packages  
      (append  
       '(auctex window-number buffer-move color-theme-tomorrow color-theme-leuven git-commit-mode magit org-mode lua-mode smtpmail-multi emacs-async)  
       (mapcar 'el-get-source-name el-get-sources))) 
; we assume mu is installed via the debian repos

; sync all the things
(el-get 'sync my-el-get-packages)

;;; LOAD OTHER FILES ;;;
(load "misc.el")
(load "bindings.el")
(load "custom.el")
(load "mu4e_config.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-custom-commands (quote (("d" todo "DELEGATED" nil) ("c" todo "DONE|DEFERRED|CANCELLED" nil) ("w" todo "WAITING" nil) ("W" agenda "" ((org-agenda-ndays 21))) ("A" agenda "" ((org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]"))) (org-agenda-ndays 1) (org-agenda-overriding-header "Today's Priority #A tasks: "))) ("u" alltodo "" ((org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote scheduled) (quote deadline) (quote regexp) "
]+>"))) (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
 '(org-agenda-files (quote ("~/org/todo.org")))
 '(org-agenda-ndays 7)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-window-setup (quote current-window))
 '(org-capture-templates (quote (("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks") "* TODO %?
  %u") ("a" "Appointment" entry (file+headline "~/org/todo.org" "Appointments") "* APPT %?
  Added: %u") ("n" "Note" entry (file+datetree "~/org/note.org") "* %? 
 Entered on %U 
 %a"))))
 '(org-deadline-warning-days 14)
 '(org-default-notes-file "~/org/notes.org")
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-log-done (quote time))
 '(org-reverse-note-order t)
 '(org-todo-keywords (quote ((sequence "TODO(t)" "STARTED(s@/!)" "WAITING(w@/!)" "DELEGATED(e@/!)" "APPT(@!)" "|" "DONE(d!)" "DEFERRED" "CANCELLED(c@)"))))
 '(send-mail-function (quote smtpmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
