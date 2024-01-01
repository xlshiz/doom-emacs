;;; editor/search/config.el -*- lexical-binding: t; -*-

(use-package! thing-edit
  :commands (thing-cut-parentheses thing-copy-parentheses thing-replace-parentheses thing-copy-region-or-line thing-cut-region-or-line thing-replace-region-or-line)
  :defer t)

(use-package! avy
  :commands (avy-goto-char-timer)
  :init
  (setq avy-all-windows nil
        avy-all-windows-alt t
        avy-background t
        ;; the unpredictability of this (when enabled) makes it a poor default
        avy-single-candidate-jump nil
        avy-timeout-seconds 0.5))

(use-package! avy-thing-edit
  :defer t)

(use-package! symbol-overlay
  :commands (symbol-overlay-put symbol-overlay-remove-all)
  :init
  (setq symbol-overlay-inhibit-map t)
  :config
  (map! :map symbol-overlay-map
        "n"  #'symbol-overlay-jump-next
        "p"  #'symbol-overlay-jump-prev))

(use-package! color-rg
  :commands (color-rg-search-input color-rg-search-symbol
              color-rg-search-symbol-in-current-file color-rg-search-project)
  :init
  (when (modulep! :completion vertico)
    (define-key! vertico-map "<M-return>" #'+search/consult-to-color-rg))
  (defconst evil-collection-color-rg-maps '(color-rg-mode-map
                                             color-rg-mode-edit-map))
  :config
  (advice-add #'color-rg-update-header-line :override #'ignore)
  (set-popup-rule! "^\\*color-rg\*"
    :height 0.4 :quit nil :select nil :ttl 0)
  (defhydra color-rg-hydra (:hint nil)
    "
    ^^^^Move               ^^^^filter                     ^^toggle            ^^change
   -^^^^-----------------+-^^^^-------------------------+-^^------------------+-^^---------------------------
    _n_   next keyword   | _r_   replace all            | _I_  toggle ignore  | _d_  change dir
    _p_   prev keyword   | _f_   filter match result    | _c_  toggle case    | _z_  change globs
    _N_   next file      | _F_   filter mismatch result | _i_  open edit mode | _Z_  change exclude
    _P_   prev file      | _x_   filter match files     | ^^                  | _t_  return literal
    _D_   remove line    | _X_   filter mismatch files  | _u_  unfilter       | _s_  return regexp
   -^^^^-----------------+-^^^^-------------------------+-^^------------------+-^^---------------------------
  "
    ("n" color-rg-jump-next-keyword)
    ("p" color-rg-jump-prev-keyword)
    ("N" color-rg-jump-next-file)
    ("P" color-rg-jump-prev-file)

    ("r" color-rg-replace-all-matches)
    ("f" color-rg-filter-match-results)
    ("F" color-rg-filter-mismatch-results)
    ("x" color-rg-filter-match-files)
    ("X" color-rg-mismatch-files)
    ("u" color-rg-unfilter)
    ("D" color-rg-remove-line-from-results)

    ("I" color-rg-rerun-toggle-ignore)
    ("t" color-rg-rerun-literal)
    ("c" color-rg-rerun-toggle-case)
    ("s" color-rg-rerun-regexp)
    ("d" color-rg-rerun-change-dir)
    ("z" color-rg-rerun-change-globs)
    ("Z" color-rg-rerun-change-exclude-files)
    ("C" color-rg-customized-search)
    ("i" color-rg-switch-to-edit-mode)
    ("q" nil "quit"))
  (after! evil-collection
    (evil-collection-color-rg-setup)))

(use-package! snails
  :when (modulep! +snails)
  :commands (snails)
  :config
  (setq snails-show-with-frame nil)
  (setq snails-fuz-library-load-status "unload")
  (setq snails-need-render-candidate-icon t)
  (setq snails-content-show-margin nil)
  (setq snails-default-backends '(snails-backend-buffer snails-backend-projectile  snails-backend-recentf))
  (setq snails-backend-buffer-blacklist (append '(" tq-temp-epdfinfo" " markdown-code-fontification") snails-backend-buffer-blacklist))
  (setq snails-prefix-backends
        '(("#" '(snails-backend-buffer))
          (">" '(snails-backend-projectile))
          ("?" '(snails-backend-recentf))
          ("%" '(snails-backend-fd))
          ("!" '(snails-backend-rg))
          ("@" '(snails-backend-imenu))
          ("$" '(snails-backend-directory-files))
          ))
  (map! :map snails-mode-map
        :i "C-j"        #'snails-select-next-item
        :i "C-k"        #'snails-select-prev-item
        :i "<down>"     #'snails-select-next-item
        :i "<up>"       #'snails-select-prev-item
        :i "M-n"        #'snails-select-next-backend
        :i "M-p"        #'snails-select-prev-backend
        :i [tab]        #'snails-candidate-tab
        :i "C-g"        #'snails-quit
        :i "<escape>"   #'snails-quit)
  (evil-set-initial-state 'snails-mode 'insert))

(after! ivy
  (defface +snail-buffer-face '((t :inherit all-the-icons-yellow)) "snail buffer")
  (defface +snail-project-face '((t :inherit all-the-icons-ivy-dir-face)) "snail project")
  (defface +snail-recent-face '((t :inherit ivy-separator)) "snail recent"))

(after! ivy-rich
  (ivy-rich-mode -1)
  (setq ivy-rich-display-transformers-list
        (plist-put ivy-rich-display-transformers-list
                   'snail
                   '(:columns
                     ((+snail-rich-buffer-tag (:width 3 :face all-the-icons-cyan))
                      (+ivy-rich-buffer-icon)
                      (+snail-colorfull-candidate (:width 0.8))))))
  (ivy-rich-mode 1))

(after! consult
  (consult-customize
   snail--source-buffer snail--source-project-file snail--source-recent-file snail--source-hidden-buffer
   +embark-find-file +embark-search-file-cwd +embark-search-file-other-dir +embark-search-file-other-project
   +embark/grep-project +embark-grep-other-cwd +embark-grep-other-project
   :preview-key "C-."))

(after! embark

  ;; Support funtion
  (defadvice! +embark-become-command-a (fn &rest args)
    :around #'embark--become-command
    (let ((command (cl-first args))
          (use-dialog-box nil)
          (input (cl-second args)))
      (if (equal (substring (format "%s" command) 0 8) "+embark-")
          (funcall command input)
        (apply fn args))
      ))

  (defvar-keymap +embark-become-snail-map
    :doc "Keymap for Embark become."
    :parent nil
    "." #'+embark-find-file
    "f" #'+embark-search-file-cwd
    "F" #'+embark-search-file-other-dir
    "P" #'+embark-search-file-other-project
    "A" #'snail)
  (add-to-list 'embark-become-keymaps '+embark-become-snail-map)

  (defvar-keymap +embark-become-grep-map
    :doc "Keymap for Embark become."
    :parent nil
    "p" #'+embark/grep-project
    "P" #'+embark-grep-other-project
    "G" #'+embark-grep-other-cwd
    "g" #'+default/search-project-for-symbol-at-point
    "b" #'+embark-grep-buffer)
  (add-to-list 'embark-become-keymaps '+embark-become-grep-map)
  )

;; Fix project.el
(defun +search|projectile-project-find-function (dir)
  (let ((root (projectile-project-root dir)))
    (and root (cons 'transient root))))
(after! project
  (add-to-list 'project-find-functions '+search|projectile-project-find-function))
