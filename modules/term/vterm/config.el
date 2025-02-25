;;; term/vterm/config.el -*- lexical-binding: t; -*-

(use-package! vterm
  :when (featurep 'dynamic-modules)
  :commands vterm-mode
  :hook (vterm-mode . doom-mark-buffer-as-real-h)
  :hook (vterm-mode . hide-mode-line-mode) ; modeline serves no purpose in vterm
  :preface
  ;; HACK Because vterm clusmily forces vterm-module.so's compilation on us when
  ;;      the package is loaded, this is necessary to prevent it when
  ;;      byte-compiling this file (`use-package' blocks eagerly loads packages
  ;;      when compiled).
  (when noninteractive
    (advice-add #'vterm-module-compile :override #'ignore)
    (provide 'vterm-module))
  ;; "C-j" to ace-window
  (defadvice evil-collection-vterm-setup (after +evil-collection-vterm-setup-h activate)
    (evil-collection-define-key 'insert 'vterm-mode-map
      (kbd "C-j") 'ace-window)
    (evil-collection-define-key 'insert 'vterm-mode-map
      (kbd "C-x C-f") 'find-file)
    (evil-collection-define-key 'normal 'vterm-mode-map
      (kbd "C-j") 'ace-window))
  (setq vterm-buffer-name "vterm*")
  :config
  ; (set-popup-rule! "^\\*vterm" :size 0.25 :vslot -4 :select t :quit nil :ttl 0)
  ; (set-popup-rule! "^\\*doom:vterm" :size 0.25 :vslot -4 :select t :quit nil :ttl 0)

  ;; "C-j" to ace-window
  (add-to-list 'vterm-keymap-exceptions "C-j")

  ;; Once vterm is dead, the vterm buffer is useless. Why keep it around? We can
  ;; spawn another if want one.
  (setq vterm-kill-buffer-on-exit t)

  ;; 5000 lines of scrollback, instead of 1000
  (setq vterm-max-scrollback 5000)

  (setq-hook! 'vterm-mode-hook
    ;; Don't prompt about dying processes when killing vterm
    confirm-kill-processes nil
    ;; Prevent premature horizontal scrolling
    hscroll-margin 0))

(map! :ni   "M-t"       #'+vterm/toggle
      :ni   [f5]        #'+vterm/toggle)
