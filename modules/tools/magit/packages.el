;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

(when (package! magit :pin "9a865f17eb2c8e32d27b7456f36b21f3879a1f8a")
  (when (featurep! +forge)
    (package! forge :pin "d9c45a90ba8b69ba9ef988e0f4421b80befa21c7"))
  (package! magit-gitflow :pin "cc41b561ec6eea947fe9a176349fb4f771ed865b")
  (package! magit-todos :pin "60152d5c4e4b73e72e15f23ca16e8cc7734906bc")
  (package! github-review :pin "341b7a1352e4ee1f1119756360ac0714abbaf460"))
