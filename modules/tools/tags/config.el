;;; tools/tags/config.el  -*- lexical-binding: t; -*-


(use-package! citre
  :defer t
  :config
  (require 'citre-config)
  (setq
    citre-project-root-function #'projectile-project-root
    citre-use-project-root-when-creating-tags t
    citre-peek-fill-fringe nil
    citre-prompt-language-for-ctags-command t))
