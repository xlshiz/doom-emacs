;; -*- no-byte-compile: t; -*-
;;; tools/lookup/packages.el

;;
(package! dumb-jump :pin "1dd583011f4025b1b8c75fd785691851b6c5dfa3")

;; For dictionary and online lookup
(package! request :pin "38ed1d2e64138eb16a9d8ed2987cff2e01b4a93b")

(when (featurep! +docsets)
  (package! dash-docs :pin "29848b6b347ac520f7646c200ed2ec36cea3feda"))

(when (featurep! +dictionary)
  (if IS-MAC
      (package! osx-dictionary :pin "1a4479d9f44ef1e6e5f7643c172c32f6fe6cce21")
    (package! define-word :pin "31a8c67405afa99d0e25e7c86a4ee7ef84a808fe")
    (package! powerthesaurus :pin "88bc5229cba1604c8f74db0a1456d99259d538cc")
    (when (featurep! +offline)
      (package! wordnut :pin "feac531404041855312c1a046bde7ea18c674915")
      (package! synosaurus :pin "14d34fc92a77c3a916b4d58400424c44ae99cd81"))))
