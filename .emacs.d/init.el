(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'bind-key)

(use-package diminish
  :ensure
  :config
  (diminish 'abbrev-mode)
  (diminish 'isearch-mode))

(use-package key-chord
  :ensure
  :config (key-chord-mode 1))

(use-package mykie
  :ensure)

(add-to-list 'load-path (locate-user-emacs-file "lisp"))


;;;
;;; variables
;;;

(setq scroll-step 10
      gc-cons-threshold (* 10 gc-cons-threshold)
      echo-keystrokes 0.1
      make-backup-files nil
      auto-save-default nil
      save-abbrevs nil
      vc-handled-backends nil
      custom-file "/dev/null")

(setq-default tab-width 2
              indent-tabs-mode nil
              truncate-partial-width-windows t
              truncate-lines t
              bidi-display-reordering nil)

(when window-system
  (add-to-list 'default-frame-alist '(width . 145))
  (add-to-list 'default-frame-alist '(height . 50)))

; Enable commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)


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
(defvar find-file-command 'counsel-find-file)
(defun find-file-from-startup-directory ()
  (interactive)
  (let ((default-directory startup-directory))
    (call-interactively find-file-command)))

(bind-keys
 ("C-a" . move-bol-or-indent)
 ("C-i" . indent-for-tab-command)
 ("C-m" . newline-and-indent)
 ("C-t" . other-window)
 ("C-z" . undo))
(define-key input-decode-map (kbd "C-h") (kbd "DEL"))

(mykie:global-set-key "C-w"
  :default (delete-region (point) (progn (backward-word) (point)))
  :region kill-region)
(mykie:global-set-key "C-x C-c"
  :default (message "Need C-u")
  :C-u! save-buffers-kill-terminal)
(mykie:global-set-key "C-x C-f"
  :default (call-interactively find-file-command)
  :C-u! find-file-from-startup-directory)
(mykie:global-set-key "C-x C-z"
  :default (message "Need C-u")
  :C-u! suspend-frame)
(mykie:global-set-key "M-%"
  :default query-replace
  :C-u! query-replace-regexp)


;;;
;;; hooks
;;;

(add-hook 'before-save-hook 'delete-trailing-whitespace)


;;;
;;; minor modes
;;;

(line-number-mode)
(column-number-mode)
(show-paren-mode)
(global-font-lock-mode)
(transient-mark-mode)
(menu-bar-mode 0)
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode 0))
(electric-pair-mode)


;;;
;;; packages
;;;

(use-package hl-line
  :config
  (setq hl-line-face 'underline)
  (global-hl-line-mode))

(use-package saveplace
  :config (save-place-mode))

(use-package recentf
  :config
  (setq recentf-max-saved-items 10000)
  (run-with-idle-timer 60 t (lambda ()
                              (let ((inhibit-message t))
                                (recentf-save-list))))
  (recentf-mode))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package which-func
  :config (which-function-mode))

(use-package ace-jump-mode
  :ensure
  :after mykie
  :commands (ace-jump-word-mode ace-jump-mode-pop-mark)
  :init
  (mykie:global-set-key "C-\\"
      :default ace-jump-word-mode
      :C-u! ace-jump-mode-pop-mark)
  :config
  (setq ace-jump-word-mode-use-query-char nil)
  (setq ace-jump-mode-move-keys (loop for i from ?a to ?z collect i)))

(use-package goto-chg
  :ensure
  :commands (goto-last-change goto-last-change-reverse)
  :init
  (key-chord-define-global "'f" 'goto-last-change)
  (key-chord-define-global "'b" 'goto-last-change-reverse))

(use-package company
  :ensure
  :diminish company-mode
  :demand
  :config
  (setq company-idle-delay .1
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil)
  (global-company-mode)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)))

(use-package popwin
  :ensure
  :config
  (add-to-list 'popwin:special-display-config '(grep-mode))
  (popwin-mode)
  :bind-keymap ("C-c p" . popwin:keymap))

(use-package expand-region
  :ensure
  :commands er/expand-region)

(use-package sequential-command
  :ensure
  :config
  (eval (macroexpand `(define-sequential-command seq-expand
                        set-mark-command ,@(make-list 100 'er/expand-region))))
  :bind (("C-@" . seq-expand)
         ("C-SPC" . seq-expand)))

(use-package flycheck
  :ensure
  :diminish
  :after popwin
  :config
  (setq flycheck-display-errors-delay 0.3)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc html-tidy)
                flycheck-highlighting-mode 'lines)
  (add-to-list 'popwin:special-display-config '("*Flycheck error messages*" :noselect t))
  (global-flycheck-mode)
  :custom-face (flycheck-error ((t (:foreground "red")))))

(use-package git-gutter
  :ensure
  :diminish
  :demand
  :config
  (setq git-gutter:handled-backends '(git hg))
  (global-git-gutter-mode)
  :bind (("C-x p" . git-gutter:previous-hunk)
         ("C-x n" . git-gutter:next-hunk)))

(use-package open-junk-file
  :ensure
  :commands open-junk-file
  :config (setq open-junk-file-format "~/junk/%Y%m%d-%H%M%S."))

(use-package subword
  :diminish superword-mode
  :config (global-superword-mode))

(use-package multiple-cursors
  :ensure)

(use-package rainbow-delimiters
  :ensure
  :hook (prog-mode . rainbow-delimiters-mode-enable))

(use-package smex
  :ensure
  :commands smex)

(use-package color-moccur
  :ensure)

(use-package atomic-chrome
  :ensure
  :if window-system
  :config (atomic-chrome-start-server))

(use-package winner
  :config (winner-mode)
  :bind (("C-q" . winner-undo)))

(use-package color-theme-solarized
  :ensure
  :unless window-system
  :config (load-theme 'solarized t))

(use-package undo-tree
  :ensure
  :diminish undo-tree-mode
  :after popwin
  :init
  (setq undo-tree-map (make-sparse-keymap))
  (bind-keys :map undo-tree-map
             ("C-z" . undo-tree-undo)
             ("C-c C-z" . undo-tree-redo)
             ("C-x u" . undo-tree-visualize))
  :config
  (add-to-list 'popwin:special-display-config '(" *undo-tree*"))
  (global-undo-tree-mode))


;;;
;;; hydra
;;;
(use-package hydra
  :ensure)

(defhydra hydra-page-move ()
  ("[" backward-page)
  ("]" forward-page))
(bind-keys ("C-x [" . hydra-page-move/backward-page)
           ("C-x ]" . hydra-page-move/forward-page))

(defhydra hydra-cursor-move ()
  ("{" backward-paragraph)
  ("}" forward-paragraph)
  ("<" beginning-of-buffer)
  (">" end-of-buffer))
(bind-keys ("M-{" . hydra-cursor-move/backward-paragraph)
           ("M-}" . hydra-cursor-move/forward-paragraph)
           ("M-<" . hydra-cursor-move/beginning-of-buffer)
           ("M->" . hydra-cursor-move/end-of-buffer))

(defhydra hydra-multiple-cursors (global-map
                                  "M-g m"
                                  :pre (multiple-cursor-mode))
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

(defhydra hydra-comint-input ()
  ("p" comint-previous-input)
  ("n" comint-next-input))
(use-package gud
  :bind (:map gud-mode-map
              ("M-p" . hydra-comint-input/comint-previous-input)
              ("M-n" . hydra-comint-input/comint-next-input)))


;;;
;;; ivy
;;;
(use-package ivy
  :ensure
  :diminish
  :config
  (setq ivy-height 20
        ivy-use-virtual-buffers t
        ivy-virtual-abbreviate 'abbreviate
        ivy-count-format "(%d/%d) "
        ivy-initial-inputs-alist '()
        ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (ivy-mode)
  :bind (("C-_" . ivy-switch-buffer)
         ("C-/" . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("M-<" . ivy-beginning-of-buffer)
         ("M->" . ivy-end-of-buffer)
         ("TAB" . ivy-dispatching-done)
         ("M-o" . ivy-occur))
  :custom-face (ivy-current-match ((t (:background "blue" :foreground "white")))))

(use-package counsel
  :ensure
  :diminish
  :config
  (setq counsel-yank-pop-height 20
        counsel-yank-pop-truncate-radius 5)
  (counsel-mode)
  :bind (("M-y" . counsel-yank-pop)
         ("C-c i" . counsel-imenu)))

(use-package ivy-rich
  :ensure
  :after (ivy counsel)
  :config
  (setq ivy-rich-path-style 'abbrev)
  (setq ivy-rich--display-transformers-list
        (plist-put ivy-rich--display-transformers-list
                   'ivy-switch-buffer
                   '(:columns
                     ((ivy-rich-candidate (:width 30))
                      (ivy-rich-switch-buffer-path))
                     :predicate
                     (lambda (cand) (get-buffer cand)))))
  (ivy-rich-reload))


;;;
;;; lisp
;;;
(setq eval-expression-print-length 1200
      eval-expression-print-level 400)

(use-package eldoc
  :diminish
  :commands eldoc-mode
  :config (setq eldoc-idle-delay 0.1)
  :hook ((emacs-lisp-mode . eldoc-mode)
         (lisp-interaction-mode . eldoc-mode)
         (ielm-mode . eldoc-mode)))

(use-package paredit
  :ensure
  :diminish
  :after mykie
  :config
  (mykie:set-keys paredit-mode-map
    "C-]"
    :default paredit-forward-slurp-sexp
    :C-u! paredit-backward-slurp-sexp
    "M-]"
    :default paredit-forward-barf-sexp
    :C-u! paredit-backward-barf-sexp)
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (lisp-interacton-mode . enable-paredit-mode)))


;;;
;;; php
;;;
(use-package php-mode
  :ensure
  :mode "\\.php\\'"
  :interpreter "php"
  :hook ((php-mode . (lambda ()
                       (c-set-offset 'topmost-intro-cont 0)
                       (c-set-offset 'statement-cont '+)))))

(use-package php-eldoc
  :ensure
  :config
  ;; ( triggers eldoc immediately
  (eval-after-load 'eldoc
    '(eldoc-add-command "autopair-insert-opening"))
  :hook (php-mode . eldoc-mode))


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
    :ensure))


;;;
;;; python
;;;
(use-package python
  :config
  (setq-default python-indent-offset 2)
  :hook (python-mode . (lambda ()
                         (setq counsel-find-file-ignore-regexp "__pycache__"))))


;;;
;;; org-mode
;;;
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-directory "~/org"
        org-startup-folded nil
        org-src-fontify-natively t)
  :bind (("C-c c" . org-capture)))


;;;
;;; misc
;;;
(use-package web-mode
  :ensure
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
  :ensure
  :mode "\\.go\\'"
  :config (setq gofmt-command "goimports")
  :hook ((before-save . gofmt-before-save)
         (python-mode . (lambda ()
                          (setq counsel-find-file-ignore-regexp "\\.test\\'")))))
(use-package markdown-mode
  :ensure
  :mode "\\.md\\'")

(use-package css-mode
  :mode "\\.css\\'"
  :config (setq css-indent-offset 2))

(use-package scss-mode
  :ensure
  :mode "\\.scss\\'"
  :config (setq css-indent-offset 2))

(use-package yaml-mode
  :ensure
  :mode "\\.yml\\'")

(use-package sh-script
  :ensure
  :mode "\\.sh\\'"
  :config (setq sh-basic-offset 2
                sh-indentation 2))
