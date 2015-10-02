(require 'use-package)

(use-package elixir-mode
  :init (add-hook 'elixir-mode-hook
                  (defun auto-activate-ruby-end-mode-for-elixir-mode ()
                    (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
                         "\\(?:^\\|\\s-+\\)\\(?:do\\)")
                    (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
                    (ruby-end-mode +1))))

(el-get-bundle alchemist)
(use-package alchemist)
