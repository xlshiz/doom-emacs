;;; tools/tags/config.el  -*- lexical-binding: t; -*-


(use-package! citre
  :init
  (global-set-key [remap evil-jump-to-tag] #'citre-jump)
  (global-set-key [remap find-tag] #'citre-jump)
  (global-set-key (kbd "M-,") #'citre-jump-back)
  (setq
    citre-project-root-function #'projectile-project-root
    citre-default-create-tags-file-location 'global-cache
    citre-use-project-root-when-creating-tags t
    citre-peek-fill-fringe nil
    citre-prompt-language-for-ctags-command t)
  :config
  (require 'citre-config))
