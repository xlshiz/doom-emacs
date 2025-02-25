;;; lang/vue/config.el -*- lexical-binding: t; -*-

(define-derived-mode vue-mode web-mode "Vue")
(delete '("\\.vue\\'" . web-mode) auto-mode-alist)
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
(when (modulep! +lsp)
  (after! lsp-mode
    (setq lsp-vetur-validation-template nil))
  (add-hook! 'vue-mode-hook #'lsp!))
