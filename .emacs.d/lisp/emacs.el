(eval-when-compile
  (require 'package)
  (require 'use-package)
  (package-initialize))

(use-package diminish
  :ensure t
  :demand t)

(use-package key-chord
  :ensure t
  :demand t
  :config (key-chord-mode 1))

(use-package mykie
  :ensure t
  :demand t)


;;;
;;; variables
;;;

(setq scroll-step 10
      gc-cons-threshold (* 10 gc-cons-threshold)
      echo-keystrokes 0.1
      make-backup-files nil
      auto-save-default nil
      save-abbrevs nil
      vc-handled-backends nil)

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

(defun move-bol-or-indent ()
  (interactive)
  (let ((pos (point)))
    (back-to-indentation)
    (if (equal pos (point))
        (move-beginning-of-line 1))))

(defvar startup-directory default-directory)
(defun find-file-from-startup-directory ()
  (interactive)
  (let ((default-directory startup-directory))
    (call-interactively 'ido-find-file)))

(bind-keys
 ("C-a" . move-bol-or-indent)
 ("C-i" . indent-for-tab-command)
 ("C-m" . newline-and-indent)
 ("C-t" . other-window)
 ("C-z" . undo)
 ("C-x C-b" . buffer-menu))
(define-key input-decode-map (kbd "C-h") (kbd "DEL"))

(mykie:global-set-key "C-w"
  :default (delete-region (point) (progn (backward-word) (point)))
  :region kill-region)
(mykie:global-set-key "C-x C-c"
  :default (message "Need C-u")
  :C-u save-buffers-kill-terminal)
(mykie:global-set-key "C-x C-f"
  :default ido-find-file
  :C-u find-file-from-startup-directory)
(mykie:global-set-key "C-x C-z"
  :default (message "Need C-u")
  :C-u suspend-frame)
(mykie:global-set-key "M-%"
  :default query-replace
  :C-u query-replace-regexp)


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
  :hook (after-init . save-place-mode)
  )

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
  :commands (ace-jump-word-mode ace-jump-mode-pop-mark)
  :init
  (mykie:global-set-key "C-\\"
      :default ace-jump-word-mode
      :C-u ace-jump-mode-pop-mark)
  :config
  (setq ace-jump-word-mode-use-query-char nil)
  (setq ace-jump-mode-move-keys (loop for i from ?a to ?z collect i)))

(use-package goto-chg
  :ensure t
  :commands (goto-last-change goto-last-change-reverse)
  :init
  (key-chord-define-global "'f" 'goto-last-change)
  (key-chord-define-global "'b" 'goto-last-change-reverse))

(use-package auto-complete-config
  :ensure auto-complete
  :diminish auto-complete-mode
  :config
  (setq ac-auto-start 2
        ac-use-menu-map t)
  (add-to-list 'ac-non-trigger-commands 'c-electric-delete-forward)
  (ac-config-default) ; set default ac-sources and allow to be overwritten
  )

(use-package smartparens
  :ensure smartparens
  :diminish smartparens-mode
  :config (require 'smartparens-config)
  :hook (after-init . smartparens-global-mode))

(use-package popwin
  :ensure t
  :config
  (setq popwin:special-display-config
        '((" *auto-async-byte-compile*" :noselect t)
          ("*grep*")
          ("*Gofmt Errors*" :noselect t)
          ("*compilation*" :noselect t)))
  :hook (after-init . popwin-mode)
  :bind-keymap ("C-c p" . popwin:keymap))

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
  :diminish yas-minor-mode
  :after auto-complete-config
  :config
  (setq-default ac-sources (cons 'ac-source-yasnippet ac-sources))
  (setq yas-buffer-local-condition
        '(if (= (point)
                (save-excursion
                  (back-to-indentation)
                  (skip-syntax-forward "^ " (line-end-position))
                  (point)))
             t
           '(require-snippet-condition . anywhere)))
  (yas-global-mode)
  (unbind-key "TAB" yas-minor-mode-map)
  (bind-key "SPC" yas-maybe-expand yas-minor-mode-map))

(use-package abbrev
  :diminish abbrev-mode)

(use-package flycheck
  :ensure t
  :diminish
  :config
  (setq flycheck-display-errors-delay 0.3)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc html-tidy))
  :hook (after-init . global-flycheck-mode)
  :custom-face (flycheck-error ((t (:foreground "red")))))

(use-package git-gutter
  :ensure t
  :diminish
  :config
  (setq git-gutter:handled-backends '(git hg))
  :hook (after-init . global-git-gutter-mode)
  :bind (("C-x p" . git-gutter:previous-hunk)
         ("C-x n" . git-gutter:next-hunk)))

(use-package open-junk-file
  :ensure t
  :commands open-junk-file
  :config (setq open-junk-file-format "~/junk/%Y%m%d-%H%M%S."))

(use-package subword
  :diminish superword-mode
  :commands superword-mode)

(use-package multiple-cursors
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode-enable))


;;;
;;; hydra
;;;
(use-package hydra
  :ensure t
  :demand t)

(defmacro defhydra-multi (hydra-name binding-prefix &rest heads)
  `(progn
     ,@(mapcar
        (lambda (head)
          (let* ((head-binding (car head))
                 (head-command (cadr head))
                 (funname (intern (format "%s-%s" hydra-name head-binding)))
                 (binding (concat binding-prefix head-binding)))
            (if (symbolp head-command)
                (setq head-command (list head-command)))
            `(progn
               (let ((hydra-fun (defhydra ,funname (:body-pre ,head-command) ,@heads)))
                 (hydra-set-property ',funname :verbosity 0)
                 (bind-key ,binding hydra-fun))))
          )
        heads)
     nil))

(defhydra-multi hydra-cursor-move "M-"
  ("{" backward-paragraph)
  ("}" forward-paragraph)
  ("<" beginning-of-buffer)
  (">" end-of-buffer))

(defhydra hydra-multiple-cursors (global-map
                                  "M-g m"
                                  (:body-pre (multiple-cursor-mode 1)))
  ("a" mc/mark-all-like-this "all")
  ("w" mc/mark-all-dwim "dwim")
  ("l" mc/edit-lines "lines")
  ("p" mc/mark-previous-like-this "mark prev")
  ("n" mc/mark-next-like-this "mark next")
  ("P" mc/skip-to-previous-like-this "skip to prev")
  ("N" mc/skip-to-next-like-this "skip to next")
  ("d" mc/unmark-next-like-this "unmark next")
  ("D" mc/unmark-previous-like-this "unmark prev")
  ("TAB" mc/cycle-forward nil)
  ("<backtab>" mc/cycle-backward nil))


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
  :diminish
  :commands eldoc-mode
  :config (setq eldoc-idle-delay 0.2)
  :hook ((emacs-lisp-mode . eldoc-mode)
         (lisp-interaction-mode . eldoc-mode)
         (ielm-mode . eldoc-mode)))

(use-package paredit
  :ensure t
  :diminish
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
  :ensure t
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
  :mode ("\\.js\\'" . js-mode)
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
