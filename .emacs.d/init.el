(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;(when load-file-name
;  (setq user-emacs-directory (file-name-directory load-file-name)))

;(load-file (locate-user-emacs-file "lisp/el-get.el"))
(load-file (locate-user-emacs-file "lisp/emacs.el"))

(byte-recompile-file (locate-user-emacs-file "init.el") nil 0)
(byte-recompile-directory (locate-user-emacs-file "lisp") 0)
