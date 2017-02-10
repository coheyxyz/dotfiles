;;;
;;; variables
;;;

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


;;;
;;; key bindings
;;;

(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-i") 'indent-for-tab-command)
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "C-t") 'other-window)
(global-set-key (kbd "C-z") 'undo)
(define-key input-decode-map (kbd "C-h") (kbd "DEL"))

(defun move-bol-or-indent ()
  (interactive)
  (let ((pos (point)))
    (back-to-indentation)
    (if (equal pos (point))
        (move-beginning-of-line 1))))

(global-set-key (kbd "C-a") 'move-bol-or-indent)

(defun kill-region-or-delete-backward-word ()
  (interactive)
  (if (or (not transient-mark-mode)
           mark-active)
      (kill-region (region-beginning) (region-end))
    (delete-region (point)
                   (progn (backward-word) (point)))))
(global-set-key (kbd "C-w") 'kill-region-or-delete-backward-word)

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
(global-set-alternate (kbd "C-x C-f")
                      'ido-find-file
                      'find-file-from-startup-directory)

(defmacro require-prefix-to-execute (key command)
  `(global-set-alternate ,key
                         (lambda ()
                           (interactive)
                           (message "Please prefix C-u to execute the command"))
                         ,command))
(require-prefix-to-execute (kbd "C-x C-c") 'save-buffers-kill-terminal)
(require-prefix-to-execute (kbd "C-x C-z") 'suspend-frame)

(global-set-alternate (kbd "M-%") 'query-replace 'query-replace-regexp)


;;;
;;; hooks
;;;

(add-hook 'before-save-hook 'delete-trailing-whitespace)


;;;
;;; minor modes
;;;

(line-number-mode 1)
(column-number-mode 1)
(show-paren-mode 1)
(global-font-lock-mode 1)
(menu-bar-mode 0)
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode 0))
(load-file (locate-user-emacs-file "lisp/color-theme.el"))


;;;
;;; packages
;;;

(require 'use-package)

(use-package ace-jump-mode
  :bind ("C-\\" . ace-jump-mode))

(use-package auto-complete-config
  :config (progn
            (ac-config-default)
            (setq ac-auto-start 2
                  ac-use-menu-map t)
            (add-to-list 'ac-non-trigger-commands 'c-electric-delete-forward)))

(use-package autopair
  :config (autopair-global-mode))

(use-package cua-base
  :init (progn
            (cua-mode t)
            (setq cua-enable-cua-keys nil))
  :bind (("C-SPC" . cua-set-mark)
         ("M-SPC" . cua-set-rectangle-mark)))

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
  :config (progn
            (global-hl-line-mode 1)
            (setq hl-line-face 'underline)))

(use-package ido
  :config (progn
            (ido-mode t)
            (ido-everywhere t)))

(use-package git-gutter
  :init (progn
          (setq git-gutter:handled-backends '(git hg))
          (global-git-gutter-mode 1))
  :bind (("C-x p" . git-gutter:previous-hunk)
         ("C-x n" . git-gutter:next-hunk)))

(use-package open-junk-file
  :commands open-junk-file
  :config (setq open-junk-file-format "~/junk/%Y%m%d-%H%M%S."))

(use-package popwin
  :config (progn
            (popwin-mode 1)
            (setq popwin:special-display-config
                  '((" *auto-async-byte-compile*" :noselect t)
                    ("*grep*")
                    ("*Gofmt Errors*" :noselect t)
                    ("*compilation*" :noselect t)))
            (global-set-key (kbd "C-c p") popwin:keymap)))

(use-package recentf
  :defer 10
  :config (progn
            (recentf-mode 1)
            (setq recentf-max-saved-items 10000)
            (run-with-idle-timer 30 t 'recentf-save-list)))

;; need to load first to keep track of commands
(use-package redo+
  :init (global-set-key (kbd "M-z") 'redo))

(use-package saveplace
  :config (setq-default save-place t))

(use-package sequential-command
  :config (progn
            (eval (macroexpand `(define-sequential-command seq-expand
                                  cua-set-mark ,@(make-list 100 'er/expand-region))))
            (global-set-key (kbd "C-@") 'seq-expand)
            (global-set-key (kbd "C-SPC") 'seq-expand)))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package which-func
  :config (which-function-mode 1))

(use-package yasnippet
  :init (progn
          (yas-global-mode 1)
          (setq-default ac-sources (append '(ac-source-yasnippet) ac-sources))
          (define-key yas-minor-mode-map (kbd "TAB") yas-maybe-expand)))


;;;
;;; helm
;;;
(use-package helm
  :config (progn
            (use-package helm-config)
            (use-package helm-ls-git)
            (setq helm-mini-default-sources '(helm-source-buffers-list
                                              helm-source-recentf
                                              helm-source-ls-git))
            (set-face-background 'helm-selection "navy"))
  :bind (("C-_" . helm-mini)
         ("C-/" . helm-mini)
         ("M-/" . helm-resume)
         ("C-c i" . helm-imenu)
         ("C-c o" . helm-occur)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)))

(use-package helm-gtags
  :bind (("M-." . helm-gtags-find-tag)
         ("M-r" . helm-gtags-find-rtag)
         ("M-," . helm-gtags-pop-stack)))

(use-package helm-ag
  :commands helm-ag)


;;;
;;; lisp
;;;
(setq eval-expression-print-length 1200
      eval-expression-print-level 400)

(add-hook 'emacs-lisp-mode-hook
          (lambda () (superword-mode)))

(use-package auto-async-byte-compile
  :commands enable-auto-async-byte-compile-mode
  :init (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)
  :config (setq auto-async-byte-compile-exclude-files-regexp "/junk/"))

(use-package eldoc
  :commands eldoc-mode
  :init (progn
          (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
          (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
          (add-hook 'ielm-mode-hook 'eldoc-mode)
          (setq eldoc-idle-delay 0.2)))

(use-package paredit
  :commands enable-paredit-mode
  :init (progn
          (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
          (add-hook 'lisp-interacton-mode-hook 'enable-paredit-mode))
  :config (progn
            (define-key paredit-mode-map (kbd "C-]") 'paredit-forward-slurp-sexp)
            (define-key paredit-mode-map (kbd "M-]") 'paredit-forward-barf-sexp)))

(use-package lispxmp
  :commands lispxmp)

; may need to update texinfo to run make
(use-package slime
  :config (setq inferior-lisp-program "sbcl"))


;;;
;;; php
;;;
(defun php-mode-do-not-align-arrows ()
  (let* ((sc (assoc 'statement-cont c-offsets-alist))
         (sc (delq 'c-lineup-cascaded-calls sc)))
    (add-to-list 'c-offsets-alist sc)))

(use-package php-mode
  :commands php-mode
  :config (progn
            (setq php-imenu-generic-expression
                  (mapcar (lambda (lst)
                            (let ((name (car lst))
                                  (exp (cadr lst))
                                  (rest (cddr lst)))
                              (setq exp (replace-regexp-in-string "function" "\\(?:async\\s-+\\)?function" exp t t))
                              (if (string-match "function" exp)
                                  (setq exp exp))
                              (append `(,name) `(,exp) rest)))
                          php-imenu-generic-expression))
            (add-hook 'php-mode-hook (lambda ()
                                       (php-mode-do-not-align-arrows)
                                       (modify-syntax-entry ?< ".")
                                       (modify-syntax-entry ?> ".")
                                       (superword-mode)))))

(use-package php-eldoc
  :init (progn
          ;; ( triggers eldoc immediately
          (eval-after-load 'eldoc
            '(eldoc-add-command "autopair-insert-opening"))
          (add-hook 'php-mode-hook 'eldoc-mode)))


;;;
;;; ruby
;;;
(use-package ruby-mode
  :mode (("\\.rb\\'" . ruby-mode)
         ("\\.rake\\'" . ruby-mode))
  :interpreter "ruby"
  :config (progn
            (setq ruby-insert-encoding-magic-comment nil)
            (setq ruby-deep-indent-paren-style nil)

            (use-package ruby-end)))

(use-package rhtml-mode
  :commands rhtml-mode
  :init (add-hook 'rhtml-mode-hook
                  (lambda ()
                    (set-face-background 'erb-face nil)
                    (set-face-bold-p 'erb-face t)
                    (set-face-background 'erb-exec-face nil)
                    (set-face-bold-p 'erb-exec-face t))))


;;;
;;; elixir
;;;
(use-package elixir-mode
  :init (add-hook 'elixir-mode-hook
                  (defun auto-activate-ruby-end-mode-for-elixir-mode ()
                    (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
                         "\\(?:^\\|\\s-+\\)\\(?:do\\)")
                    (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
                    (ruby-end-mode +1))))

(use-package alchemist)


;;;
;;; python
;;;
(use-package python
  :config (progn
            (setq-default python-indent-offset 2)
            (add-to-list 'auto-mode-alist '("\\.j2\\'" . web-mode))
            (if (boundp 'web-mode-engines-alist)
                (add-to-list 'web-mode-engines-alist '("django" . "\\.j2\\'"))
              (setq web-mode-engines-alist '(("django" . "\\.j2\\'"))))
            (add-hook 'python-mode-hook
                      (lambda ()
                        (setq ido-ignore-files (cons "__pycache__" ido-ignore-files))))
            (add-hook 'web-mode-hook
                      (lambda ()
                        (if (string= web-mode-engine "django")
                            (push ?{ (getf autopair-dont-pair :code)))))))

(use-package ein)


;;;
;;; misc
;;;
(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.twig\\'" . web-mode)
         ("\\.eex\\'" . web-mode)
         ("\\.gohtml\\'" . web-mode))
  :config (progn
            (add-hook 'web-mode-hook
                      (lambda ()
                        (custom-set-faces
                         '(web-mode-function-call-face ((t :inherit default)))
                         '(web-mode-html-tag-face ((t :inherit 'font-lock-keyword-face)))
                         '(web-mode-html-attr-name-face ((t :inherit 'default)))
                         '(web-mode-html-tag-bracket-face ((t :inherit 'default))))
                        (setq web-mode-script-padding 2
                              web-mode-style-padding 2
                              web-mode-markup-indent-offset 2
                              web-mode-css-indent-offset 2
                              web-mode-code-indent-offset 2
                              web-mode-attr-indent-offset 2
                              web-mode-enable-auto-indentation t
                              web-mode-enable-auto-closing t
                              web-mode-enable-auto-opening t
                              web-mode-enable-auto-pairing t
                              web-mode-auto-close-style 2)
                        (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))))))

(use-package js
  :config (progn
            (setq js-indent-level 2)))

(use-package go-mode
  :commands go-mode
  :init (progn
          (setq ido-ignore-files (cons "\\.test\\'" ido-ignore-files)))
  :config (progn
            (setq gofmt-command "goimports")
            (add-hook 'before-save-hook 'gofmt-before-save)))

(use-package markdown-mode
  :commands markdown-mode)

(use-package scss-mode
  :mode (("\\.scss\\'" . scss-mode))
  :config (progn
            (setq css-indent-offset 2)))

(use-package yaml-mode
  :commands yaml-mode)

(use-package sh-mode
  :commands shell-mode
  :config (progn
            (setq sh-basic-offset 2
                  sh-indentation 2)))
