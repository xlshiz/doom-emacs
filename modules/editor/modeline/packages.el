;; -*- no-byte-compile: t; -*-
;;; ui/modeline/packages.el

(unless (or (featurep! +light) (featurep! +mini))
  (package! doom-modeline :pin "ce9899f00af40edb78f58b9af5c3685d67c8eed2"))
(when (featurep! +mini)
  (package! awesome-tray :recipe (:host github :repo "manateelazycat/awesome-tray" :files ("*.el") :build (autoloads))))
(package! anzu :pin "5abb37455ea44fa401d5f4c1bdc58adb2448db67")
(when (featurep! :editor evil)
  (package! evil-anzu :pin "d3f6ed4773b48767bd5f4708c7f083336a8a8a86"))
