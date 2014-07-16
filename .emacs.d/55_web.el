(require 'use-package)

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
            (add-hook 'web-mode-hook (lambda ()
                                       (setq web-mode-script-padding 2
                                             web-mode-style-padding 2)))))
