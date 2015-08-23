(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'use-package)

(use-package init-loader
  :init (init-loader-load "~/.emacs.d/lisp"))

(byte-recompile-directory (expand-file-name "~/.emacs.d/lisp") 0)
