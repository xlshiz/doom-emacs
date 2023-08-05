;; -*- no-byte-compile: t; -*-
;;; editor/hl/packages.el

(package! hl-todo :pin "a627d33214e60fc5a2fe6164411d908e1afdba59")
(if (modulep! +diff-hl)
    (package! diff-hl :pin "d20f16bf5eadd66e775f215e800f25caddae8cb5")
  (package! git-gutter-fringe :pin "648cb5b57faec55711803cdc9434e55a733c3eba"))
