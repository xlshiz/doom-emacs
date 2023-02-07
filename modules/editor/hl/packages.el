;; -*- no-byte-compile: t; -*-
;;; editor/hl/packages.el

(package! hl-todo :pin "e52285965b5ee89c18080661d4f80270143ae8dc")
(if (modulep! +diff-hl)
    (package! diff-hl :pin "dabb7be6283488abd8d232ea8ce590d502713ed8")
  (package! git-gutter-fringe :pin "648cb5b57faec55711803cdc9434e55a733c3eba"))
