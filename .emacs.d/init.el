(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; suppress minor warnings while loading packages
(setq ad-redefinition-action 'accept)

(load-file (locate-user-emacs-file "lisp/el-get.el"))
(load-file (locate-user-emacs-file "lisp/emacs.el"))

(byte-recompile-directory (locate-user-emacs-file "lisp") 0)
