;;
;; variables
;;

(setq scroll-step 1
      gc-cons-threshold (* 10 gc-cons-threshold)
      echo-keystrokes 0.1
      make-backup-files nil
      auto-save-default nil
      save-abbrevs nil)

(setq-default tab-width 2
              indent-tabs-mode nil
              truncate-partial-width-windows t
              truncate-lines t)


;;
;; key bindings
;;

(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-unset-key (kbd "C-x C-z"))
(global-set-key (kbd "C-x C-z z") 'suspend-frame)
(global-set-key (kbd "C-i") 'indent-for-tab-command)
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "C-t") 'other-window)
(global-set-key (kbd "C-z") 'undo)
(define-key input-decode-map (kbd "C-h") (kbd "DEL"))

(defun move-bol-or-indent ()
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (move-beginning-of-line 1)))
(global-set-key (kbd "C-a") 'move-bol-or-indent)

(defun kill-region-or-backward-word ()
  (interactive)
  (if (or (not transient-mark-mode)
           mark-active)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))
(global-set-key (kbd "C-w") 'kill-region-or-backward-word)

(defmacro global-set-alternate (key regular alternate)
  `(global-set-key ,key (lambda (prefix)
                          (interactive "P")
                          (call-interactively
                           (if prefix
                               ,alternate
                             ,regular)))))

(defvar startup-directory default-directory)
(defun find-file-from-startup-directory ()
  (interactive)
  (let ((default-directory startup-directory))
    (call-interactively 'ido-find-file)))
(global-set-alternate (kbd "C-x C-f") 'ido-find-file 'find-file-from-startup-directory)

(global-set-alternate (kbd "M-%") 'query-replace 'query-replace-regexp)


;;
;; hooks
;;

(add-hook 'before-save-hook 'delete-trailing-whitespace)


;;
;; minor modes
;;

(line-number-mode 1)
(column-number-mode 1)
(show-paren-mode 1)
(global-font-lock-mode 1)
(menu-bar-mode 0)
(if (fboundp 'tool-bar-mode) (tool-bar-mode 0))


;;
;; packages
;;

(require 'use-package)

(use-package auto-complete-config
  :init (ac-config-default)
  :config (progn
            (setq ac-auto-start 2
                  ac-use-menu-map t)
            (add-to-list 'ac-non-trigger-commands 'c-electric-delete-forward)))

(use-package autopair
  :init (autopair-global-mode))

(use-package cua-base
  :init (cua-mode t)
  :config (setq cua-enable-cua-keys nil)
  :bind ("M-SPC" . cua-set-rectangle-mark))

(use-package expand-region
  :commands er/expand-region)

(use-package flycheck
  :init (progn
          (add-hook 'after-init-hook 'global-flycheck-mode))
  :config (progn
            (setq flycheck-display-errors-delay 0.3)
            (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc html-tidy))
            (set-face-foreground 'flycheck-error "red")))

(use-package hl-line
  :init (global-hl-line-mode 1)
  :config (setq hl-line-face 'underline))

(use-package ido
  :init (progn
          (ido-mode t)
          (ido-everywhere t)))

(use-package git-gutter
  :init (global-git-gutter-mode 1)
  :bind (("M-p" . git-gutter:previous-hunk)
         ("M-n" . git-gutter:next-hunk)))

(use-package open-junk-file
  :commands open-junk-file
  :config (setq open-junk-file-format "~/junk/%Y%m%d-%H%M%S."))

(use-package popwin
  :init (popwin-mode 1)
  :config (progn (setq popwin:special-display-config
                       '((" *auto-async-byte-compile*" :noselect t)
                         ("*grep*")))
                 (global-set-key (kbd "C-c p") popwin:keymap)))

(use-package recentf
  :init (recentf-mode 1)
  :config (progn
            (setq recentf-max-saved-items 10000)
            (run-with-idle-timer 30 t 'recentf-save-list)))

; need to load first to keep track of commands
(use-package redo+
  :init (global-set-key (kbd "M-z") 'redo))

(use-package saveplace
  :config (setq-default save-place t))

(use-package sequential-command
  :init (progn
          (define-sequential-command seq-expand
            cua-set-mark
            er/expand-region er/expand-region er/expand-region er/expand-region
            er/expand-region er/expand-region er/expand-region er/expand-region
            er/expand-region er/expand-region er/expand-region er/expand-region
            er/expand-region er/expand-region er/expand-region er/expand-region)
          (global-set-key (kbd "C-@") 'seq-expand)
          (global-set-key (kbd "C-SPC") 'seq-expand)))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package which-func
  :init (which-function-mode 1))

(use-package yasnippet
  :init (yas-global-mode 1))
