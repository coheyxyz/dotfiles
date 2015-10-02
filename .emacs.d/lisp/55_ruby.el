(require 'use-package)

(el-get-bundle ruby-mode)
(use-package ruby-mode
  :mode (("\\.rb\\'" . ruby-mode)
         ("\\.rake\\'" . ruby-mode))
  :interpreter "ruby"
  :config (progn
            (setq ruby-insert-encoding-magic-comment nil)
            (setq ruby-deep-indent-paren-style nil)

            (use-package ruby-end)

            (use-package rinari
              :init (global-rinari-mode)
              :config (defadvice jump-select-and-find-file (after reenable-updir activate compile)
                        (define-key ido-file-dir-completion-map "\C-h" 'ido-delete-backward-updir)))
            ))

(el-get-bundle rhtml-mode)
(use-package rhtml-mode
  :commands rhtml-mode
  :init (add-hook 'rhtml-mode-hook
                  (lambda ()
                    (rinari-launch)
                    (set-face-background 'erb-face nil)
                    (set-face-bold-p 'erb-face t)
                    (set-face-background 'erb-exec-face nil)
                    (set-face-bold-p 'erb-exec-face t))))
