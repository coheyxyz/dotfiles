(require 'use-package)

(defun self-insert-command-and-indent (num)
  (interactive "p")
  (self-insert-command num)
  (indent-according-to-mode))

(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.twig\\'" . web-mode))
  :config (progn
            (custom-set-faces
             '(web-mode-function-call-face ((t :inherit default)))
             '(web-mode-html-tag-face ((t :inherit 'font-lock-keyword-face)))
             '(web-mode-html-attr-name-face ((t :inherit 'default)))
             '(web-mode-html-tag-bracket-face ((t :inherit 'default))))
            (setq web-mode-script-padding 2
                  web-mode-style-padding 2
                  web-mode-enable-auto-indentation t
                  web-mode-tag-auto-close-style 2)
            (define-key web-mode-map (kbd "<") 'self-insert-command-and-indent)
            (define-key web-mode-map (kbd ">") 'self-insert-command-and-indent)))
