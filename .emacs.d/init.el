(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(add-to-list 'load-path (locate-user-emacs-file "lisp"));
(load "emacs")

(byte-recompile-file (locate-user-emacs-file "init.el") nil 0)
(byte-recompile-directory (locate-user-emacs-file "lisp") 0)
