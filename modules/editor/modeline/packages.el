;; -*- no-byte-compile: t; -*-
;;; ui/modeline/packages.el

(unless (or (modulep! +light) (modulep! +mini))
  (package! doom-modeline :pin "173ad0a27f2c3babe2009a3b760b92d3c8718f5a"))
(when (modulep! +mini)
  (package! awesome-tray :recipe (:host github :repo "manateelazycat/awesome-tray" :files ("*.el") :build (autoloads))))
(package! anzu :pin "5abb37455ea44fa401d5f4c1bdc58adb2448db67")
(when (modulep! :editor evil)
  (package! evil-anzu :pin "d1e98ee6976437164627542909a25c6946497899"))
