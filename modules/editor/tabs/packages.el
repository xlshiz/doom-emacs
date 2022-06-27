;; -*- no-byte-compile: t; -*-
;;; editor/tabs/packages.el

(package! awesome-tab :recipe (:host github :repo "xlshiz/awesome-tab" :files ("*.el") :build (autoloads)))
(package! sort-tab :recipe (:host github :repo "manateelazycat/sort-tab" :files ("*.el") :build (autoloads)))
(package! toki-tabs :recipe (:host github :repo "xlshiz/toki-tabs.el" :files ("*.el") :build (autoloads)))
