;; -*- no-byte-compile: t; -*-
;;; editor/chinese/packages.el

(package! pinyinlib.el :recipe (:host github :repo "xlshiz/pinyinlib.el"))
(package! ace-pinyin :recipe (:host github :repo "xlshiz/ace-pinyin"))
(package! evil-pinyin :recipe (:host github :repo "laishulu/evil-pinyin"))
(package! pyim :pin "0e8c1c5ab5a1fc2015036e6ef0e6147b7e1dc225")
(if (featurep! +rime)
    (package! liberime :pin "8d4d1d4f2924dc560bce1d79680df36dcc086d49" :recipe (:host github :repo "merrickluo/liberime" :files ("CMakeLists.txt" "Makefile" "src" "liberime*.el" "liberime.el")))
  (package! pyim-basedict))
(package! pangu-spacing)
(package! company-english-helper :recipe (:host github :repo "manateelazycat/company-english-helper"))
(package! insert-translated-name :recipe (:host github :repo "manateelazycat/insert-translated-name"))
(package! sis :pin "f2d4031711714b100ec81aac321917c40cf20dc9" :recipe (:host github :repo "laishulu/emacs-smart-input-source"))
