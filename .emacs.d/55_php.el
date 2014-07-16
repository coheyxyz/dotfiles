(require 'use-package)

(defun php-mode-do-not-align-arrows ()
  (let* ((sc (assoc 'statement-cont c-offsets-alist))
         (sc (delq 'c-lineup-cascaded-calls sc)))
    (add-to-list 'c-offsets-alist sc)))

(use-package php-mode
  :commands php-mode
  :config (progn
            (add-hook 'php-mode-hook 'php-mode-do-not-align-arrows)))

(use-package php-eldoc
  :init (progn
          ; ( triggers eldoc immediately
          (eval-after-load 'eldoc
            '(eldoc-add-command "c-electric-paren"))
          (add-hook 'php-mode-hook 'php-eldoc-enable)))

(use-package geben)
