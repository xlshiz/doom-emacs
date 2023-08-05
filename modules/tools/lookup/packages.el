;; -*- no-byte-compile: t; -*-
;;; tools/lookup/packages.el

;;
(package! dumb-jump :pin "d9503c157ab88f0ed2fa1301aeb57e95ac564760")

;; For dictionary and online lookup
(package! request :pin "01e338c335c07e4407239619e57361944a82cb8a")

(when (modulep! +docsets)
  (package! dash-docs :pin "29848b6b347ac520f7646c200ed2ec36cea3feda"))

(when (modulep! +dictionary)
  (if IS-MAC
      (package! osx-dictionary :pin "0715e5a3ac659df32a0f0fabfbbeef0228fbd9a9")
    (package! define-word :pin "31a8c67405afa99d0e25e7c86a4ee7ef84a808fe")
    ;; REVIEW: This fork fixes SavchenkoValeriy/emacs-powerthesaurus#40.
    (package! powerthesaurus
      :recipe (:host github
               :repo "doomelpa/powerthesaurus")
      :pin "d9ebb866f6fce469102665f187266f0a041cfc4b")
    (when (modulep! +offline)
      (package! wordnut :pin "feac531404041855312c1a046bde7ea18c674915")
      (package! synosaurus :pin "14d34fc92a77c3a916b4d58400424c44ae99cd81"))))
