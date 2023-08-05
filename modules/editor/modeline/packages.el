;; -*- no-byte-compile: t; -*-
;;; ui/modeline/packages.el

(unless (or (modulep! +light) (modulep! +mini))
  (package! doom-modeline :pin "6125309c2caa3c98591a4c802e9b4dd2f7ea83e9"))
(when (modulep! +mini)
  (package! awesome-tray :recipe (:host github :repo "manateelazycat/awesome-tray" :files ("*.el") :build (autoloads))))
(package! anzu :pin "5abb37455ea44fa401d5f4c1bdc58adb2448db67")
(when (modulep! :editor evil)
  (package! evil-anzu :pin "d1e98ee6976437164627542909a25c6946497899"))
