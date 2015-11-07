(require 'use-package)

(el-get-bundle go-mode)
(use-package go-mode
  :commands go-mode
  :init (progn
          (setq ido-ignore-files (cons "\\.test\\'" ido-ignore-files)))
  :config (progn
            (setq gofmt-command "goimports")
            (add-hook 'before-save-hook 'gofmt-before-save)))

(el-get-bundle markdown-mode)
(use-package markdown-mode
  :commands markdown-mode)

(el-get-bundle scss-mode)
(use-package scss-mode
  :mode (("\\.scss\\'" . scss-mode))
  :config (progn
            (setq css-indent-offset 2)))

(el-get-bundle yaml-mode)
(use-package yaml-mode
  :commands yaml-mode)

(use-package sh-mode
  :commands shell-mode
  :config (progn
            (setq sh-basic-offset 2
                  sh-indentation 2)))

(el-get-bundle python-mode)
(use-package python-mode)
