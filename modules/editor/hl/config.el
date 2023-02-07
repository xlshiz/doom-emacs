;;; editor/hl/config.el -*- lexical-binding: t; -*-

;;;{{{1 defvar
;; TODO Implement me
(defvar +vc-gutter-in-margin nil
  "If non-nil, use the margin for diffs instead of the fringe.")

(defvar +vc-gutter-in-remote-files nil
  "If non-nil, enable the vc gutter in remote files (e.g. open through TRAMP).")

;;;}}}
;;;{{{1 package
;;;{{{2 hl-todo
(use-package! hl-todo
  :hook (prog-mode . hl-todo-mode)
  :hook (yaml-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        '(;; For reminders to change or add something at a later date.
          ("TODO" warning bold)
          ;; For code (or code paths) that are broken, unimplemented, or slow,
          ;; and may become bigger problems later.
          ("FIXME" error bold)
          ;; For code that needs to be revisited later, either to upstream it,
          ;; improve it, or address non-critical issues.
          ("REVIEW" font-lock-keyword-face bold)
          ;; For code smells where questionable practices are used
          ;; intentionally, and/or is likely to break in a future update.
          ("HACK" font-lock-constant-face bold)
          ;; For sections of code that just gotta go, and will be gone soon.
          ;; Specifically, this means the code is deprecated, not necessarily
          ;; the feature it enables.
          ("DEPRECATED" font-lock-doc-face bold)
          ;; Extra keywords commonly found in the wild, whose meaning may vary
          ;; from project to project.
          ("NOTE" success bold)
          ("BUG" error bold)
          ("XXX" font-lock-constant-face bold)))


  (defadvice! +hl-todo-clamp-font-lock-fontify-region-a (fn &rest args)
    "Fix an `args-out-of-range' error in some modes."
    :around #'hl-todo-mode
    (letf! (defun font-lock-fontify-region (beg end &optional loudly)
             (funcall font-lock-fontify-region (max beg 1) end loudly))
      (apply fn args)))

  ;; Use a more primitive todo-keyword detection method in major modes that
  ;; don't use/have a valid syntax table entry for comments.
  (add-hook! '(pug-mode-hook haml-mode-hook)
    (defun +hl-todo--use-face-detection-h ()
      "Use a different, more primitive method of locating todo keywords."
      (set (make-local-variable 'hl-todo-keywords)
           '(((lambda (limit)
                (let (case-fold-search)
                  (and (re-search-forward hl-todo-regexp limit t)
                       (memq 'font-lock-comment-face (ensure-list (get-text-property (point) 'face))))))
              (1 (hl-todo-get-face) t t))))
      (when hl-todo-mode
        (hl-todo-mode -1)
        (hl-todo-mode +1)))))
;;;}}}

;;;{{{2 git-gutter
;;
;;; git-gutter

(use-package! git-gutter
  :commands git-gutter:revert-hunk git-gutter:stage-hunk
  :init
  (add-hook! 'find-file-hook
    (defun +vc-gutter-init-maybe-h ()
      "Enable `git-gutter-mode' in the current buffer.
If the buffer doesn't represent an existing file, `git-gutter-mode's activation
is deferred until the file is saved. Respects `git-gutter:disabled-modes'."
      (let ((file-name (buffer-file-name (buffer-base-buffer))))
        (cond
         ((and (file-remote-p (or file-name default-directory))
               (not +vc-gutter-in-remote-files)))
         ;; UX: If not a valid file, wait until it is written/saved to activate
         ;;   git-gutter.
         ((not (and file-name (vc-backend file-name)))
          (add-hook 'after-save-hook #'+vc-gutter-init-maybe-h nil 'local))
         ;; UX: Allow git-gutter or git-gutter-fringe to activate based on the
         ;;   type of frame we're in. This allows git-gutter to work for silly
         ;;   geese who open both tty and gui frames from the daemon.
         ((if (and (display-graphic-p)
                   (require 'git-gutter-fringe nil t))
              (setq-local git-gutter:init-function      #'git-gutter-fr:init
                          git-gutter:view-diff-function #'git-gutter-fr:view-diff-infos
                          git-gutter:clear-function     #'git-gutter-fr:clear
                          git-gutter:window-width -1)
            (setq-local git-gutter:init-function      'nil
                        git-gutter:view-diff-function #'git-gutter:view-diff-infos
                        git-gutter:clear-function     #'git-gutter:clear-diff-infos
                        git-gutter:window-width 1))
          (unless (memq major-mode git-gutter:disabled-modes)
            (git-gutter-mode +1)
            (remove-hook 'after-save-hook #'+vc-gutter-init-maybe-h 'local)))))))

  ;; UX: Disable in Org mode, as per syl20bnr/spacemacs#10555 and
  ;;   syohex/emacs-git-gutter#24. Apparently, the mode-enabling function for
  ;;   global minor modes gets called for new buffers while they are still in
  ;;   `fundamental-mode', before a major mode has been assigned. I don't know
  ;;   why this is the case, but adding `fundamental-mode' here fixes the issue.
  (setq git-gutter:disabled-modes '(fundamental-mode image-mode pdf-view-mode))
  :config
  (set-popup-rule! "^\\*git-gutter" :select nil :size '+popup-shrink-to-fit)

  ;; PERF: Only enable the backends that are available, so it doesn't have to
  ;;   check when opening each buffer.
  (setq git-gutter:handled-backends
        (cons 'git (cl-remove-if-not #'executable-find (list 'hg 'svn 'bzr)
                                     :key #'symbol-name)))

  ;; UX: update git-gutter on focus (in case I was using git externally)
  (add-hook 'focus-in-hook #'git-gutter:update-all-windows)

  (add-hook! '(doom-escape-hook doom-switch-window-hook) :append
    (defun +vc-gutter-update-h (&rest _)
      "Refresh git-gutter on ESC. Return nil to prevent shadowing other
`doom-escape-hook' hooks."
      (ignore (or (memq this-command '(git-gutter:stage-hunk
                                       git-gutter:revert-hunk))
                  inhibit-redisplay
                  (if git-gutter-mode
                      (git-gutter)
                    (+vc-gutter-init-maybe-h))))))
  ;; UX: update git-gutter when using magit commands
  (advice-add #'magit-stage-file   :after #'+vc-gutter-update-h)
  (advice-add #'magit-unstage-file :after #'+vc-gutter-update-h)

  ;; FIX: stop git-gutter:{next,previous}-hunk from jumping to random hunks.
  (defadvice! +vc-gutter--fix-linearity-of-hunks-a (diffinfos is-reverse)
    :override #'git-gutter:search-near-diff-index
    (cl-position-if (let ((lineno (line-number-at-pos))
                          (fn (if is-reverse #'> #'<)))
                      (lambda (line) (funcall fn lineno line)))
                    diffinfos
                    :key #'git-gutter-hunk-start-line
                    :from-end is-reverse)))
;;;}}}

;;;{{{2 git-gutter-fringe
(use-package! git-gutter-fringe
  :defer t
  :config
  ;; standardize default fringe width
  (if (fboundp 'fringe-mode) (fringe-mode '4))

  ;; places the git gutter outside the margins.
  (setq-default fringes-outside-margins t)
  ;; thin fringe bitmaps
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom))
;;;}}}

;;;}}}

;;; vim:filetype=lisp foldmethod=marker
