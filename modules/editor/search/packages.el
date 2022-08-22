;; -*- no-byte-compile: t; -*-
;;; editor/search/packages.el

(package! avy :pin "ba5f035be33693d1a136a5cbeedb24327f551a92")
(package! thing-edit :recipe (:host github :repo "manateelazycat/thing-edit"))
(package! avy-thing-edit :recipe (:host github :repo "xlshiz/avy-thing-edit"))
(package! imenu-list)
(package! symbol-overlay)
(package! color-rg :recipe (:host github :repo "manateelazycat/color-rg" :build (autoloads)))
(when (modulep! +snails)
  (package! snails :recipe (:host github :repo "xlshiz/snails" :files ("*.el" "*.sh") :build (autoloads))))
