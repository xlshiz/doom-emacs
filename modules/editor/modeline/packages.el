;; -*- no-byte-compile: t; -*-
;;; ui/modeline/packages.el

(unless (or (featurep! +light) (featurep! +mini))
  (package! doom-modeline :pin "69ede7d719764f26671897c5020f295e5eb1e6c8"))
(when (featurep! +mini)
  (package! awesome-tray :recipe (:host github :repo "manateelazycat/awesome-tray" :files ("*.el") :build (autoloads))))
(package! anzu :pin "5abb37455ea44fa401d5f4c1bdc58adb2448db67")
(when (featurep! :editor evil)
  (package! evil-anzu :pin "d3f6ed4773b48767bd5f4708c7f083336a8a8a86"))
