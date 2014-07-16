(require 'use-package)

(use-package ruby-mode
  :commands ruby-mode
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
          (add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
          (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode)))
  :config (progn
            (use-package ruby-end)
            (setq ruby-insert-encoding-magic-comment nil)
            (setq ruby-deep-indent-paren-style nil)))

(use-package rinari
  :init (global-rinari-mode)
  :config (defadvice jump-select-and-find-file (after reenable-updir activate compile)
            (define-key ido-file-dir-completion-map "\C-h" 'ido-delete-backward-updir)))

(use-package rhtml-mode
  :init (add-hook 'rhtml-mode-hook
                  (lambda ()
                    (rinari-launch)
                    (set-face-background 'erb-face nil)
                    (set-face-bold-p 'erb-face t)
                    (set-face-background 'erb-exec-face nil)
                    (set-face-bold-p 'erb-exec-face t))))
