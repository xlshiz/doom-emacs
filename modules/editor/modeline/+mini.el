;;; ui/modeline/+mini.el -*- lexical-binding: t; -*-

;; Warning: this is still a WIP!

(use-package! awesome-tray
  :hook (after-init . awesome-tray-mode)
  :config
  (setq awesome-tray-active-modules
        '("evil" "buffer-name"  "location" "mode-name" "git"))
  )
