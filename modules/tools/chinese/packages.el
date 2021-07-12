;; -*- no-byte-compile: t; -*-
;;; editor/chinese/packages.el

(package! pinyinlib.el :recipe (:host github :repo "xlshiz/pinyinlib.el"))
(package! ace-pinyin :recipe (:host github :repo "xlshiz/ace-pinyin"))
(package! evil-pinyin :recipe (:host github :repo "laishulu/evil-pinyin"))
(package! pyim :pin "bacb6251e6f3ca1910fce6f8b8098750a6f85428")
(if (featurep! +rime)
    (package! liberime :pin "8d4d1d4f2924dc560bce1d79680df36dcc086d49" :recipe (:host github :repo "merrickluo/liberime" :files ("CMakeLists.txt" "Makefile" "src" "liberime*.el" "liberime.el")))
  (package! pyim-basedict))
(package! pangu-spacing)
(package! company-english-helper :recipe (:host github :repo "manateelazycat/company-english-helper"))
(package! insert-translated-name :recipe (:host github :repo "manateelazycat/insert-translated-name"))
(package! sis :recipe (:host github :repo "laishulu/emacs-smart-input-source"))
