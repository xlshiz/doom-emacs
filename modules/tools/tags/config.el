;;; tools/tags/config.el  -*- lexical-binding: t; -*-


(use-package! citre
  :init
  (setq
   citre-project-root-function #'projectile-project-root
   citre-default-create-tags-file-location 'project-cache
   citre-use-project-root-when-creating-tags nil
   ;; citre-peek-fill-fringe nil
   citre-prompt-language-for-ctags-command t)
  :defer 0.1
  :config
  (require 'citre-config)
  (map! :n "g C-]"  #'citre-peek
        :n "C-]"    #'citre-jump
        :n "M-,"    #'citre-jump-back)
  (map! (:map citre-peek-keymap
         "M-n"     #'citre-peek-next-line
         "M-p"     #'citre-peek-prev-line
         "M-N"     #'citre-peek-next-definition
         "M-P"     #'citre-peek-prev-definition
         :n [right]   #'citre-peek-chain-forward
         :n [left]    #'citre-peek-chain-backward
         :n [up]      #'citre-peek-prev-branch
         :n [down]    #'citre-peek-next-branch
         "M-m p"   #'citre-peek-through
         "M-m d"   #'citre-peek-delete-branch
         "M-m D"   #'citre-peek-delete-branches
         :n [S-up]    #'citre-peek-move-current-def-up
         :n [S-down]  #'citre-peek-move-current-def-down
         "M-m f"   #'citre-peek-make-current-def-first
         "M-m j"   #'citre-peek-jump
         :n "q"       #'citre-peek-abort)))
