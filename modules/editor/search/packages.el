;; -*- no-byte-compile: t; -*-
;;; editor/search/packages.el

(package! avy :pin "be612110cb116a38b8603df367942e2bb3d9bdbe")
(package! thing-edit :recipe (:host github :repo "manateelazycat/thing-edit"))
(package! avy-thing-edit :recipe (:host github :repo "xlshiz/avy-thing-edit"))
(package! symbol-overlay)
(package! color-rg :recipe (:host github :repo "manateelazycat/color-rg" :build (autoloads)))
(when (modulep! +snails)
  (package! snails :recipe (:host github :repo "xlshiz/snails" :files ("*.el" "*.sh") :build (autoloads))))
