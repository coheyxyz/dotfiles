(require 'use-package)

(el-get-bundle auto-async-byte-compile)
(use-package auto-async-byte-compile
  :commands enable-auto-async-byte-compile-mode
  :init (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)
  :config (setq auto-async-byte-compile-exclude-files-regexp "/junk/"))

(use-package eldoc
  :commands turn-on-eldoc-mode
  :init (progn
          (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
          (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
          (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
          (setq eldoc-idle-delay 0.2)))

(el-get-bundle paredit)
(use-package paredit
  :commands enable-paredit-mode
  :init (progn
          (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
          (add-hook 'lisp-interacton-mode-hook 'enable-paredit-mode))
  :config (progn
            (define-key paredit-mode-map (kbd "C-j") 'eval-print-last-sexp)
            (define-key paredit-mode-map (kbd "C-]") 'paredit-forward-slurp-sexp)
            (define-key paredit-mode-map (kbd "M-]") 'paredit-forward-barf-sexp)))

(el-get-bundle lispxmp)
(use-package lispxmp
  :commands lispxmp)
