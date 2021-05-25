;; -*- no-byte-compile: t; -*-
;;; editor/search/packages.el

(package! symbol-overlay)
(package! color-rg :recipe (:host github :repo "manateelazycat/color-rg" :build (autoloads)))
(when (featurep! +snails)
  (package! snails :recipe (:host github :repo "xlshiz/snails" :files ("*.el" "*.sh") :build (autoloads))))
