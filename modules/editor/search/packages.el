;; -*- no-byte-compile: t; -*-
;;; editor/search/packages.el

(package! avy :pin "955c8dedd68c74f3cf692c1249513f048518c4c9")
(package! thing-edit :recipe (:host github :repo "manateelazycat/thing-edit"))
(package! avy-thing-edit :recipe (:host github :repo "xlshiz/avy-thing-edit"))
(package! symbol-overlay)
(package! color-rg :recipe (:host github :repo "manateelazycat/color-rg" :build (autoloads)))
(when (modulep! +snails)
  (package! snails :recipe (:host github :repo "xlshiz/snails" :files ("*.el" "*.sh") :build (autoloads))))
