;; -*- no-byte-compile: t; -*-
;;; lang/cc/packages.el

(package! cmake-mode
  :recipe (:host github :repo "emacsmirror/cmake-mode" :files (:defaults "*"))
  :pin "d5d77de8c4c69e348b182eeb30222b2f1ba8db7b")
(package! cuda-mode :pin "7f593518fd135fc6af994024bcb47986dfa502d2")
(package! demangle-mode :pin "aaef0bd77a3ea9ce9132e9a53ac021b0f5d33e12")
(package! disaster :pin "10a785facc60d89d78e0d5177985ab1af1741bb4")
(package! modern-cpp-font-lock :pin "43c6b68ff58fccdf9deef11674a172e4eaa8455c")
(package! opencl-mode :pin "15091eff92c33ee0d1ece40eb99299ef79fee92d")

(when (package! glsl-mode :pin "b07112016436d9634cd4ef747f9af6b01366d136")
  (when (featurep! :completion company)
    (package! company-glsl
      :recipe (:host github :repo "Kaali/company-glsl")
      :pin "404cd0694ab34971f9c01eb22126cd2e7d3f9dc4")))

(package! ggtags)
(package! counsel-gtags)

(if (featurep! +lsp)
    (unless (featurep! :tools lsp +eglot)
      ;; ccls package is necessary only for lsp-mode.
      (package! ccls :pin "675a5704c14a27931e835a431beea3631d92e8e6")))
