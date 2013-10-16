; first off get the nice el-get package manager
; as it makes everything much easier
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(add-to-list 'load-path "~/.emacs.d")

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
       '(auctex window-number buffer-move color-theme-tomorrow magit org-mode lua-mode matlab-mode)  
       (mapcar 'el-get-source-name el-get-sources))) 

; sync all the things
(el-get 'sync my-el-get-packages)

;;; LOAD OTHER FILES ;;;
(load "misc.el")
(load "bindings.el")
(load "custom.el")


