;;; lang/graphql/config.el -*- lexical-binding: t; -*-

(after! graphql-mode
  (defface nerd-icons-rhodamine
    '((t (:foreground "#E10098")))
    "Face for GraphQL icon."
    :group 'nerd-icons-faces)
  (if (modulep! +lsp)
      (add-hook 'graphql-mode-local-vars-hook #'lsp! 'append)
    (set-company-backend! 'graphql-mode 'company-graphql))

  (add-hook 'graphql-mode-hook #'rainbow-delimiters-mode)
  (set-docsets! 'graphql-mode :add "GraphQL Specification")

  (set-electric! 'graphql-mode
    :chars '(?\} ?\))
    :words '("or" "and"))

  )

(use-package! graphql-doc
  :after graphql-mode)
