(require 'use-package)

(use-package helm
  :config (progn
            (use-package helm-config)
            (use-package helm-ls-git)
            (setq helm-mini-default-sources '(helm-source-buffers-list
                                              helm-source-recentf
                                              helm-source-ls-git))
            (set-face-background 'helm-selection "navy"))
  :bind (("C-_" . helm-mini)
         ("M-/" . helm-resume)
         ("C-c i" . helm-imenu)
         ("C-c o" . helm-occur)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)))

(use-package helm-gtags
  :bind (("M-." . helm-gtags-find-tag)
         ("M-r" . helm-gtags-find-rtag)
         ("M-," . helm-gtags-pop-stack)))

(use-package helm-ag
  :commands helm-ag)
