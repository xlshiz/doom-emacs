;; -*- no-byte-compile: t; -*-
;;; editor/tabs/packages.el

(package! awesome-tab :recipe (:host github :repo "manateelazycat/awesome-tab" :files ("*.el") :build (autoloads)) :pin "1dc909488f893d1fe8bb113120da4b47b7d3fc39")
(package! sort-tab :recipe (:host github :repo "manateelazycat/sort-tab" :files ("*.el") :build (autoloads)))
