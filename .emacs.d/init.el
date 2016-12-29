;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; suppress minor warnings while loading packages
(setq ad-redefinition-action 'accept)

(load-file (locate-user-emacs-file "lisp/el-get.el"))
(load-file (locate-user-emacs-file "lisp/emacs.el"))

(byte-recompile-directory (locate-user-emacs-file "lisp") 0)
