;;; ui/modeline/+mini.el -*- lexical-binding: t; -*-

;; Warning: this is still a WIP!

(use-package! awesome-tray
  :hook (after-init . awesome-tray-mode)
  :config
  (defun my-location-info ()
    (format "%s:%s"
            (format-mode-line "%l")
            (format-mode-line "%c")
            ))
  (add-to-list 'awesome-tray-module-alist
               '("my-location" . (my-location-info awesome-tray-module-date-face)))
  (defun my-lsp-info ()
    (with-demoted-errors
        ""
      (if (featurep 'lsp-mode)
          (if (lsp-workspaces)
              "lsp"
            ""))))
  (add-to-list 'awesome-tray-module-alist
               '("my-lsp" . (my-lsp-info awesome-tray-module-circe-face)))
  (setq awesome-tray-active-modules
        '("file-path" "my-location" "mode-name" "my-lsp" "git" "evil"))
  )
