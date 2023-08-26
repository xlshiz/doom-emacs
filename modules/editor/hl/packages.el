;; -*- no-byte-compile: t; -*-
;;; editor/hl/packages.el

(package! hl-todo :pin "0faf8569b67f5b23891416d9e7a67e3843338f2a")
(if (modulep! +diff-hl)
    (package! diff-hl :pin "b5651f1c57b42e0f38e01a8fc8c7df9bc76d5d38")
  (package! git-gutter-fringe :pin "648cb5b57faec55711803cdc9434e55a733c3eba"))
