(require 'use-package)

(defun php-mode-do-not-align-arrows ()
  (let* ((sc (assoc 'statement-cont c-offsets-alist))
         (sc (delq 'c-lineup-cascaded-calls sc)))
    (add-to-list 'c-offsets-alist sc)))

(el-get-bundle php-mode)
(use-package php-mode
  :commands php-mode
  :config (progn
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
            (add-hook 'php-mode-hook 'php-mode-do-not-align-arrows)))

(el-get-bundle php-eldoc)
(use-package php-eldoc
  :init (progn
          ; ( triggers eldoc immediately
          (eval-after-load 'eldoc
            '(eldoc-add-command "c-electric-paren"))
          (add-hook 'php-mode-hook 'php-eldoc-enable)))

(setq ac-modes (cons 'xhp-mode ac-modes))

(el-get-bundle geben)
(use-package geben)
