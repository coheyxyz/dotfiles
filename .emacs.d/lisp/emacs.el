(eval-when-compile
  (require 'package)
  (require 'use-package)
  (package-initialize))


;;;
;;; variables
;;;

(setq scroll-step 10
      gc-cons-threshold (* 10 gc-cons-threshold)
      echo-keystrokes 0.1
      make-backup-files nil
      auto-save-default nil
      save-abbrevs nil)

(setq-default tab-width 2
              indent-tabs-mode nil
              truncate-partial-width-windows t
              truncate-lines t
              bidi-display-reordering nil)

; Enable commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


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
(transient-mark-mode 1)
(menu-bar-mode 0)
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode 0))
(load-file (locate-user-emacs-file "lisp/color-theme.el"))


;;;
;;; packages
;;;

(use-package cua-base
  :config
  (setq cua-enable-cua-keys nil)
  :hook (after-init . cua-mode)
  :bind (("C-SPC" . cua-set-mark)
         ("M-SPC" . cua-set-rectangle-mark)))

(use-package hl-line
  :config
  (setq hl-line-face 'underline)
  :hook (after-init . global-hl-line-mode))

(use-package ido
  :config
  (setq ido-everywhere t)
  :hook (after-init . ido-mode))

(use-package saveplace
  :hook (after-init . save-place-mode))

(use-package recentf
  :config
  (setq recentf-max-saved-items 10000)
  (run-with-idle-timer 30 t 'recentf-save-list)
  :hook (after-init . recentf-mode))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package which-func
  :hook (after-init . which-function-mode))

(use-package ace-jump-mode
  :ensure t
  :bind ("C-\\" . ace-jump-word-mode)
  :config (setq ace-jump-word-mode-use-query-char nil))

(use-package auto-complete-config
  :ensure auto-complete
  :config
  (setq ac-auto-start 2
        ac-use-menu-map t)
  (add-to-list 'ac-non-trigger-commands 'c-electric-delete-forward)
  :hook (after-init . ac-config-default))

(use-package smartparens-config
  :ensure smartparens)

(use-package popwin
  :ensure t
  :config
  (setq popwin:special-display-config
        '((" *auto-async-byte-compile*" :noselect t)
          ("*grep*")
          ("*Gofmt Errors*" :noselect t)
          ("*compilation*" :noselect t)))
  :hook (after-init . popwin-mode)
  :bind ("C-c p" . popwin:keymap))

(use-package expand-region
  :ensure t
  :commands er/expand-region)

(use-package sequential-command
  :ensure t
  :config
  (message "seq config")
  (eval (macroexpand `(define-sequential-command seq-expand
                        cua-set-mark ,@(make-list 100 'er/expand-region))))
  :bind (("C-@" . seq-expand)
         ("C-SPC" . seq-expand))
  :defines org-mode-map)

(use-package yasnippet
  :ensure t
  :after auto-complete-config
  :bind (:map yas-minor-mode-map
              ("TAB" . yas-maybe-expand))
  :hook (after-init . yas-global-mode))

(use-package flycheck
  :ensure t
  :config
  (setq flycheck-display-errors-delay 0.3)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc html-tidy))
  :hook (after-init . global-flycheck-mode)
  :custom-face (flycheck-error ((t (:foreground "red")))))

(use-package git-gutter
  :ensure t
  :config
  (setq git-gutter:handled-backends '(git hg))
  :hook (after-init . global-git-gutter-mode)
  :bind (("C-x p" . git-gutter:previous-hunk)
         ("C-x n" . git-gutter:next-hunk)))

(use-package open-junk-file
  :ensure t
  :commands open-junk-file
  :config (setq open-junk-file-format "~/junk/%Y%m%d-%H%M%S."))


;;;
;;; helm
;;;
(use-package helm
  :ensure t
  :config
  (setq helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-recentf))
  (if window-system
      (set-face-foreground 'helm-selection "white"))
  :bind (("C-_" . helm-mini)
         ("C-/" . helm-mini)
         ("M-/" . helm-resume)
         ("C-c i" . helm-imenu)
         ("C-c o" . helm-occur)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring))
  :custom-face (helm-selection ((t (:background "navy")))))


;;;
;;; lisp
;;;
(setq eval-expression-print-length 1200
      eval-expression-print-level 400)

(add-hook 'emacs-lisp-mode-hook
          (lambda () (superword-mode)))

(use-package auto-async-byte-compile
  :ensure t
  :hook (emacs-lisp-mode . enable-auto-async-byte-compile-mode))

(use-package eldoc
  :commands eldoc-mode
  :config (setq eldoc-idle-delay 0.2)
  :hook ((emacs-lisp-mode . eldoc-mode)
         (lisp-interaction-mode . eldoc-mode)
         (ielm-mode . eldoc-mode)))

(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (lisp-interacton-mode . enable-paredit-mode))
  :bind (:map paredit-mode-map
              ("C-]" . paredit-forward-slurp-sexp)
              ("M-]" . paredit-forward-barf-sexp)))


;;;
;;; php
;;;
(defun php-mode-do-not-align-arrows ()
  (let* ((sc (assoc 'statement-cont c-offsets-alist))
         (sc (delq 'c-lineup-cascaded-calls sc)))
    (add-to-list 'c-offsets-alist sc)))

(use-package php-mode
  :mode "\\.php\\'"
  :interpreter "php"
  :config
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
  :hook ((php-mode . (lambda ()
                       (php-mode-do-not-align-arrows)
                       (modify-syntax-entry ?< ".")
                       (modify-syntax-entry ?> ".")
                       (superword-mode)))))

(use-package php-eldoc
  :ensure t
  :config
  ;; ( triggers eldoc immediately
  (eval-after-load 'eldoc
    '(eldoc-add-command "autopair-insert-opening"))
  :hook ((php-mode . eldoc-mode)))


;;;
;;; ruby
;;;
(use-package ruby-mode
  :mode "\\.\\(rb\\|rake\\)\\'"
  :interpreter "ruby"
  :config
  (setq ruby-insert-encoding-magic-comment nil
        ruby-deep-indent-paren-style nil)
  (use-package ruby-end
    :ensure t))


;;;
;;; python
;;;
(use-package python
  :config
  (setq-default python-indent-offset 2)
  :hook ((python-mode . (lambda ()
                          (setq ido-ignore-files (cons "__pycache__" ido-ignore-files))))))


;;;
;;; misc
;;;
(use-package web-mode
  :ensure t
  :mode "\\.html\\'"
  :config
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
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  :custom-face
  (web-mode-function-call-face ((t :inherit default)))
  (web-mode-html-tag-face ((t :inherit 'font-lock-keyword-face)))
  (web-mode-html-attr-name-face ((t :inherit 'default)))
  (web-mode-html-tag-bracket-face ((t :inherit 'default))))

(use-package js
  :mode "\\.js\\'"
  :config (setq js-indent-level 2))

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :config (setq gofmt-command "goimports")
  :hook ((before-save . gofmt-before-save)
         (python-mode . (lambda ()
                          (setq ido-ignore-files (cons "\\.test\\'" ido-ignore-files))))))

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'")

(use-package scss-mode
  :ensure t
  :mode "\\.scss\\'"
  :config (setq css-indent-offset 2))

(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'")

(use-package sh-script
  :ensure t
  :mode "\\.sh\\'"
  :config (setq sh-basic-offset 2
                sh-indentation 2))
