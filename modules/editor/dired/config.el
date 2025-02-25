;;; editor/dired/config.el -*- lexical-binding: t; -*-

(defvar +dired-dirvish-icon-provider 'nerd-icons
  "Icon provider to use for dirvish when the module is enabled.")

(use-package! dired
  :commands dired-jump
  :init
  (setq dired-dwim-target t  ; suggest a target for moving/copying intelligently
        dired-hide-details-hide-symlink-targets nil
        ;; don't prompt to revert, just do it
        dired-auto-revert-buffer #'dired-buffer-stale-p
        ;; Always copy/delete recursively
        dired-recursive-copies  'always
        dired-recursive-deletes 'top
        ;; Ask whether destination dirs should get created when copying/removing files.
        dired-create-destination-dirs 'ask
        ;; Where to store image caches
        image-dired-dir (concat doom-cache-dir "image-dired/")
        image-dired-db-file (concat image-dired-dir "db.el")
        image-dired-gallery-dir (concat image-dired-dir "gallery/")
        image-dired-temp-image-file (concat image-dired-dir "temp-image")
        image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")
        ;; Screens are larger nowadays, we can afford slightly larger thumbnails
        image-dired-thumb-size 150)
  (after! recentf (remove-hook 'dired-mode-hook #'doom--recentf-add-dired-directory-h))
  :config
  (set-popup-rule! "^\\*image-dired"
    :slot 20 :size 0.8 :select t :quit nil :ttl 0)
  (set-evil-initial-state! 'image-dired-display-image-mode 'emacs)

  (let ((args (list "-ahl" "-v" "--group-directories-first")))
    (when IS-BSD
      ;; Use GNU ls as `gls' from `coreutils' if available. Add `(setq
      ;; dired-use-ls-dired nil)' to your config to suppress the Dired warning
      ;; when not using GNU ls.
      (if-let (gls (executable-find "gls"))
          (setq insert-directory-program gls)
        ;; BSD ls doesn't support -v or --group-directories-first
        (setq args (list (car args)))))
    (setq dired-listing-switches (string-join args " "))

    (add-hook! 'dired-mode-hook
      (defun +dired-disable-gnu-ls-flags-maybe-h ()
        "Remove extraneous switches from `dired-actual-switches' when it's
uncertain that they are supported (e.g. over TRAMP or on Windows).

Fixes #1703: dired over TRAMP displays a blank screen.
Fixes #3939: unsortable dired entries on Windows."
        (when (or (file-remote-p default-directory)
                  (and (boundp 'ls-lisp-use-insert-directory-program)
                       (not ls-lisp-use-insert-directory-program)))
          (setq-local dired-actual-switches (car args))))))

  ;; Don't complain about this command being disabled when we use it
  (put 'dired-find-alternate-file 'disabled nil)

  (defadvice! +dired--no-revert-in-virtual-buffers-a (&rest args)
    "Don't auto-revert in dired-virtual buffers (see `dired-virtual-revert')."
    :before-while #'dired-buffer-stale-p
    (not (eq revert-buffer-function #'dired-virtual-revert)))
  ;; Use single buffer
  (defadvice dired-find-file (around dired-find-file-single-buffer activate)
    "Replace current buffer if file is a directory."
    (interactive)
    (let ((orig (current-buffer))
          (filename (dired-get-file-for-visit)))
      ad-do-it
      (when (and (file-directory-p filename)
                 (not (eq (current-buffer) orig)))
        (kill-buffer orig))))
  (defadvice dired-up-directory (around dired-up-directory-single-buffer activate)
    "Replace current buffer if file is a directory."
    (interactive)
    (let ((orig (current-buffer)))
      ad-do-it
      (kill-buffer orig)))

  (map! :map dired-mode-map
        :ng "h"   #'dired-up-directory
        :ng "j"   #'dired-next-line
        :ng "k"   #'dired-previous-line
        :ng "l"   #'dired-find-file
        :ng "i"   #'wdired-change-to-wdired-mode
        :ng "."   #'dired-omit-mode
        ;; Kill all dired buffers on q
        :ng "q" #'+dired/quit-all
        ;; To be consistent with ivy/helm+wgrep integration
        "C-c C-e" #'wdired-change-to-wdired-mode))


(use-package! dired-rsync
  :general (dired-mode-map "C-c C-r" #'dired-rsync))


(use-package! diredfl
  :hook (dired-mode . diredfl-mode))


(use-package! ranger
  :when (modulep! +ranger)
  :after dired
  :init (setq ranger-override-dired t)
  :config
  (unless (file-directory-p image-dired-dir)
    (make-directory image-dired-dir))

  (set-popup-rule! "^\\*ranger" :ignore t)

  (defadvice! +dired--cleanup-header-line-a ()
    "Ranger fails to clean up `header-line-format' when it is closed, so..."
    :before #'ranger-revert
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (equal header-line-format '(:eval (ranger-header-line)))
            (setq header-line-format nil))))))

  (defadvice! +dired--cleanup-mouse1-bind-a ()
    "Ranger binds an anonymous function to mouse-1 after previewing a buffer
that prevents the user from escaping the window with the mouse. This command is
never cleaned up if the buffer already existed before ranger was initialized, so
we have to clean it up ourselves."
    :after #'ranger-setup-preview
    (when (window-live-p ranger-preview-window)
      (with-current-buffer (window-buffer ranger-preview-window)
        (local-unset-key [mouse-1]))))

  (defadvice! +dired--ranger-travel-a ()
    "Temporary fix for this function until ralesi/ranger.el#236 gets merged."
    :override #'ranger-travel
    (interactive)
    (let ((prompt "Travel: "))
      (cond
       ((bound-and-true-p helm-mode)
        (ranger-find-file (helm-read-file-name prompt)))
       ((bound-and-true-p ivy-mode)
        (ivy-read prompt 'read-file-name-internal
                  :matcher #'counsel--find-file-matcher
                  :action
                  (lambda (x)
                    (with-ivy-window
                     (ranger-find-file (expand-file-name x default-directory))))))
       ((bound-and-true-p ido-mode)
        (ranger-find-file (ido-read-file-name prompt)))
       (t
        (ranger-find-file (read-file-name prompt))))))

  (setq ranger-cleanup-on-disable t
        ranger-excluded-extensions '("mkv" "iso" "mp4")
        ranger-deer-show-details t
        ranger-max-preview-size 10
        ranger-show-literal nil
        ranger-hide-cursor nil))


(use-package! dirvish
  :when (modulep! +dirvish)
  :defer t
  :init (after! dired (dirvish-override-dired-mode))
  :hook (dired-mode . dired-omit-mode)
  :custom
  ;; Go back home? Just press `bh'
  (dirvish-bookmark-entries
    '(("h" "~/"                          "Home")
      ("d" "~/Downloads/"                "Downloads")
      ("j" "~/workdir/src/jd/"           "JD")))
  :config
  ;; (dirvish-peek-mode)
  ;; Dired options are respected except a few exceptions, see FAQ.org
  (setq dirvish-cache-dir (concat doom-cache-dir "dirvish/")
        dirvish-hide-details t
        dirvish-attributes '(subtree-state collapse file-size file-time)
        dirvish-side-auto-expand t
        dirvish-subtree-state-style 'nerd
        dirvish-reuse-session nil
        dirvish-use-mode-line nil
        dirvish-header-line-height '(1 . 1)
        delete-by-moving-to-trash nil
        dired-recursive-deletes 'always
        dired-dwim-target t
        dired-omit-files (concat "\\`[.]?#\\|\\`[.][.]?\\'"
                                 "\\|^\\..*"
                                 "\\|\\(?:\\.js\\)?\\.meta\\'"
                                 "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'"))
  (setq dirvish-mode-line-format
    '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
  (when (modulep! +icons)
    (push +dired-dirvish-icon-provider dirvish-attributes))
  (defadvice dirvish-find-entry-a (around dirvish-find-file-single-buffer activate)
    "Replace current buffer if file is a directory."
    (interactive)
    (let ((orig (current-buffer))
          (filename (dired-get-file-for-visit)))
      ad-do-it
      (when (and (file-directory-p filename)
                 (not (eq (current-buffer) orig)))
        (kill-buffer orig))))
  (map! :map dired-mode-map
    :ng "h"   #'dired-up-directory
    :ng "j"   #'dired-next-line
    :ng "k"   #'dired-previous-line
    :ng "l"   #'dired-find-file
    :ng "i"   #'wdired-change-to-wdired-mode
    :ng "."   #'dired-omit-mode
    :ng "TAB" #'dirvish-subtree-toggle
    :ng "M-n" #'dirvish-history-go-forward
    :ng "M-p" #'dirvish-history-go-backward
    :ng "M-s" #'dirvish-setup-menu
    :ng "M-f" #'dirvish-toggle-fullscreen
    :ng "*"   #'dirvish-mark-menu
    :ng "r"   #'dirvish-fd-roam
    :ng "b"   #'dirvish-bookmark-jump
    :ng "z"   #'dirvish-show-history
    :ng "f"   #'dirvish-file-info-menu
    :ng [remap dired-sort-toggle-or-edit] #'dirvish-quicksort
    :ng [remap dired-do-redisplay] #'dirvish-ls-switches-menu
    :ng [remap dired-summary] #'dirvish-dispatch
    :ng [remap dired-do-copy] #'dirvish-yank-menu
    :ng [remap mode-line-other-buffer] #'dirvish-history-last)
  (set-popup-rule! "^ \\*Dirvish.*" :ignore t)
  (global-set-key [remap find-dired] #'dirvish-fd))


(use-package! nerd-icons-dired
  :when (modulep! +icons)
  :unless (modulep! +dirvish)
  :hook (dired-mode . nerd-icons-dired-mode)
  :config
  (defadvice! +dired-disable-icons-in-wdired-mode-a (&rest _)
    :before #'wdired-change-to-wdired-mode
    (setq-local +wdired-icons-enabled (if nerd-icons-dired-mode 1 -1))
    (when nerd-icons-dired-mode
      (nerd-icons-dired-mode -1)))

  (defadvice! +dired-restore-icons-after-wdired-mode-a (&rest _)
    :after #'wdired-change-to-dired-mode
    (nerd-icons-dired-mode +wdired-icons-enabled)))


(use-package! dired-x
  :unless (modulep! +dirvish)
  :unless (modulep! +ranger)
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-verbose nil
        dired-omit-files
        (concat dired-omit-files
                "\\|^\\.DS_Store\\'"
                "\\|^\\.project\\(?:ile\\)?\\'"
                "\\|^\\.\\(?:svn\\|git\\)\\'"
                "\\|^\\.ccls-cache\\'"
                "\\|\\(?:\\.js\\)?\\.meta\\'"
                "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'"))
  ;; Disable the prompt about whether I want to kill the Dired buffer for a
  ;; deleted directory. Of course I do!
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  ;; Let OS decide how to open certain files
  (when-let (cmd (cond (IS-MAC "open")
                       (IS-LINUX "xdg-open")
                       (IS-WINDOWS "start")))
    (setq dired-guess-shell-alist-user
          `(("\\.\\(?:docx\\|pdf\\|djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpe?g\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
            ("\\.html?\\'" ,cmd)
            ("\\.md\\'" ,cmd))))
  (map! :map dired-mode-map
        :localleader
        "h" #'dired-omit-mode))


(use-package! fd-dired
  :when doom-projectile-fd-binary
  :defer t
  :init
  (global-set-key [remap find-dired] #'fd-dired)
  (set-popup-rule! "^\\*F\\(?:d\\|ind\\)\\*$" :ignore t))

(use-package! dired-aux
  :defer t
  :config
  (setq dired-create-destination-dirs 'ask
        dired-vc-rename-file t))

;;;###package dired-git-info
(map! :after dired
      :map (dired-mode-map ranger-mode-map)
      :ng ")" #'dired-git-info-mode)
(setq dgi-commit-message-format "%h %cs %s"
      dgi-auto-hide-details-p nil)
(after! wdired
  ;; Temporarily disable `dired-git-info-mode' when entering wdired, due to
  ;; reported incompatibilities.
  (defvar +dired--git-info-p nil)
  (defadvice! +dired--disable-git-info-a (&rest _)
    :before #'wdired-change-to-wdired-mode
    (setq +dired--git-info-p (bound-and-true-p dired-git-info-mode))
    (when +dired--git-info-p
      (dired-git-info-mode -1)))
  (defadvice! +dired--reactivate-git-info-a (&rest _)
    :after '(wdired-exit
             wdired-abort-changes
             wdired-finish-edit)
    (when +dired--git-info-p
      (dired-git-info-mode +1))))
