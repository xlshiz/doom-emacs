;;; editor/tabs/config.el -*- lexical-binding: t; -*-

(use-package! awesome-tab
  :unless (modulep! +sort)
  :hook (doom-init-ui . awesome-tab-mode)
  :init
  (setq awesome-tab-display-icon t)
  (setq awesome-tab-height 140)
  (setq awesome-tab-cycle-scope 'tabs)
  (setq awesome-tab-display-sticky-function-name nil)
  (setq awesome-tab-ace-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq awesome-tab-dark-unselected-blend 0.9
        awesome-tab-dark-selected-foreground-color "#55ced1"
        awesome-tab-dark-unselected-foreground-color "#AAAAAA")
  (setq awesome-tab-light-unselected-blend 0.9
        awesome-tab-light-selected-foreground-color "#44AD8E"
        awesome-tab-light-unselected-foreground-color "#555555")
  :config
  (setq uniquify-buffer-name-style 'forward)

  (defun +tabs-buffer-groups-fn ()
    (list
     (cond
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((or (derived-mode-p 'org-mode)
           (derived-mode-p 'org-agenda-mode))
       "org-mode")
      ((or (string-equal "*scratch*" (buffer-name))
           (string-equal "*Messages*" (buffer-name))
           (derived-mode-p 'pdf-view-mode))
       "Emacs")
      (t "Files"))))
  (setq awesome-tab-buffer-groups-function #'+tabs-buffer-groups-fn
        awesome-tab-buffer-list-function #'+tabs-buffer-list)


  (defhydra tabs-fast-switch (:hint nil)
    "
    ^^^^Fast Move             ^^^^Tab                    ^^Search            ^^Misc
   -^^^^--------------------+-^^^^---------------------+-^^----------------+-^^---------------------------
      ^_k_^   prev group    | _C-a_^^     select first | _g_ search group  | _C-S-k_  kill all buffer in group
    _h_   _l_  switch tab   | _C-e_^^     select last  | _d_ kill buffer   | _C-k_    kill others in group
      ^_j_^   next group    | _C-j_^^     ace jump     | ^^                | ^^
    ^^0 ~ 9^^ select window | _C-h_/_C-l_ move current | ^^                | ^^
   -^^^^--------------------+-^^^^---------------------+-^^----------------+-^^---------------------------
  "
    ("h" awesome-tab-backward-tab)
    ("j" awesome-tab-forward-group)
    ("k" awesome-tab-backward-group)
    ("l" awesome-tab-forward-tab)
    ("C-a" awesome-tab-select-beg-tab)
    ("C-e" awesome-tab-select-end-tab)
    ("C-j" awesome-tab-ace-jump)
    ("C-h" awesome-tab-move-current-tab-to-left)
    ("C-l" awesome-tab-move-current-tab-to-right)
    ("g" awesome-tab-counsel-switch-group)
    ("d" kill-current-buffer)
    ("C-S-k" awesome-tab-kill-all-buffers-in-current-group)
    ("C-k" awesome-tab-kill-other-buffers-in-current-group)
    ("q" nil "quit")))

(use-package! sort-tab
  :when (modulep! +sort)
  :defer 0.2
  :config
  (window-divider-mode -1)
  (setq uniquify-buffer-name-style 'forward)
  (defhydra tabs-fast-switch (:hint nil)
    "
    ^^^^Tab                    ^^Misc
   -^^^^---------------------+-^^^^---------------------------
    _C-a_^^     select first | _C-k_^^   close tab
    _C-e_^^     select last  | _C-j_^^   ace jump
    _h_   _l_  switch tab    | _C-S-k_  close all tabs
   -^^^^---------------------+-^^^^---------------------------
  "
    ("h" sort-tab-select-prev-tab)
    ("l" sort-tab-select-next-tab)
    ("C-a" sort-tab-select-first-tab)
    ("C-e" sort-tab-select-last-tab)
    ("C-k" sort-tab-close-current-tab)
    ("C-S-k" sort-tab-close-all-tabs)
    ("C-j" sort-tab-ace-jump)
    ("q" nil "quit"))
  (setq sort-tab-height 20)
  (sort-tab-mode t)
  (defadvice! +sort-tab-buffer-need-hide (fn &rest args)
    :around #'sort-tab-buffer-need-hide-p
    (let ((ret (apply fn args)))
      (if ret
          ret
        (with-current-buffer (car args)
          (or (derived-mode-p 'dired-mode)
              (derived-mode-p 'pdf-view-mode)))))))
