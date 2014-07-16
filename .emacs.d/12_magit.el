(require 'use-package)

(use-package magit
  :config (progn
            (set-face-background 'magit-item-highlight "#333333")
            (set-face-foreground 'magit-diff-add "#40ff40")
            (set-face-background 'magit-diff-add "#333333")
            (set-face-foreground 'magit-diff-del "#ff4040")
            (set-face-background 'magit-diff-del "#333333")
            (set-face-foreground 'magit-diff-none "#ffffff")
            (set-face-foreground 'magit-diff-hunk-header "black")))
