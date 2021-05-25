;;; editor/evil/+ex.el -*- lexical-binding: t; -*-

(use-package! imenu-list
  :defer t
  :config
  (setq imenu-list-focus-after-activation t
        imenu-list-idle-update-delay 0.5
        imenu-list-auto-resize t)
  (set-popup-rule! "^\\*Ilist"
    :side 'right :size 35 :quit nil :select nil :ttl 0))

(use-package! thing-edit
  :commands (thing-cut-parentheses thing-copy-parentheses thing-replace-parentheses thing-copy-region-or-line thing-cut-region-or-line thing-replace-region-or-line)
  :defer t)

(use-package! avy
  :commands (avy-goto-char-timer)
  :init
  (setq avy-timeout-seconds 0.5))

(use-package! avy-thing-edit
  :defer t
  :config
  (setq avy-thing-edit-jump-command #'evil-avy-goto-char-timer))
