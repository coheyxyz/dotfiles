(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(el-get-bundle use-package)
(require 'use-package)

(el-get-bundle init-loader)
(use-package init-loader
  :init (init-loader-load "~/.emacs.d/lisp"))

(byte-recompile-directory (expand-file-name "~/.emacs.d/lisp") 0)
