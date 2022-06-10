;;; editor/misc/config.el -*- lexical-binding: t; -*-

;;;{{{1 defvar
(defvar-local +electric-indent-words '()
  "The list of electric words. Typing these will trigger reindentation of the
current line.")

(defvar +default-want-RET-continue-comments t
  "If non-nil, RET will continue commented lines.")

(unless IS-WINDOWS
  (setq tramp-default-method "ssh")) ; faster than the default scp

;;;}}}
;;;{{{1 package
;;;{{{2 hl-todo
(use-package! hl-todo
  :hook (prog-mode . hl-todo-mode)
  :hook (yaml-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        '(;; For missing features or functionality that should be added at a
          ;; later date.
          ("TODO" warning bold)
          ;; For code (or code paths) that are broken or slow, and may become
          ;; bigger problems later.
          ("FIXME" error bold)
          ;; For code smells, where questionable coding practices are
          ;; intentionally used, and/or may break in a future update.
          ("HACK" font-lock-constant-face bold)
          ;; For things that need confirmation that they work or more testing.
          ("REVIEW" font-lock-keyword-face bold)
          ;; For things that just gotta go and will soon be gone.
          ("DEPRECATED" font-lock-doc-face bold)
          ;; These are extra, commonly seen annotation keywords. What they mean
          ;; or  are for depend on the project.
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
                       (memq 'font-lock-comment-face (doom-enlist (get-text-property (point) 'face))))))
              (1 (hl-todo-get-face) t t))))
      (when hl-todo-mode
        (hl-todo-mode -1)
        (hl-todo-mode +1)))))
;;;}}}

;;;{{{2 git-gutter
(use-package! git-gutter
  :commands git-gutter:revert-hunk git-gutter:stage-hunk
  :init
  (defvar +vc-gutter-in-margin nil
    "If non-nil, use the margin for diffs instead of the fringe.")

  (defvar +vc-gutter-in-remote-files nil
    "If non-nil, enable the vc gutter in remote files (e.g. open through TRAMP).")

  (defvar +vc-gutter-diff-unsaved-buffer nil
    "If non-nil, `diff-hl-flydiff-mode' will be activated. This
allows on-the-fly diffing, even for unsaved buffers.")

  (defvar +vc-gutter-default-style t
    "If non-nil, enable the default look of the vc gutter.
This means subtle thin bitmaps on the left, an arrow bitmap for
flycheck, and flycheck indicators moved to the right fringe.")
  (add-hook! 'find-file-hook
    (defun +vc-gutter-init-maybe-h ()
      "Enable `git-gutter-mode' in the current buffer.
If the buffer doesn't represent an existing file, `git-gutter-mode's activation
is deferred until the file is saved. Respects `git-gutter:disabled-modes'."
      (let ((file-name (buffer-file-name (buffer-base-buffer))))
        (cond
         ((and (file-remote-p (or file-name default-directory))
               (not +vc-gutter-in-remote-files)))
         ;; If not a valid file, wait until it is written/saved to activate
         ;; git-gutter.
         ((not (and file-name (vc-backend file-name)))
          (add-hook 'after-save-hook #'+vc-gutter-init-maybe-h nil 'local))
         ;; Allow git-gutter or git-gutter-fringe to activate based on the type
         ;; of frame we're in. This allows git-gutter to work for silly geese
         ;; who open both tty and gui frames from the daemon.
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
  (after! flycheck
    (when +vc-gutter-default-style
      ;; let diff have left fringe, flycheck can have right fringe
      (setq flycheck-indication-mode 'right-fringe)
      ;; A non-descript, left-pointing arrow
      (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
        [16 48 112 240 112 48 16] nil nil 'center)))
  ;; Disable in Org mode, as per syl20bnr/spacemacs#10555 and
  ;; syohex/emacs-git-gutter#24. Apparently, the mode-enabling function for
  ;; global minor modes gets called for new buffers while they are still in
  ;; `fundamental-mode', before a major mode has been assigned. I don't know why
  ;; this is the case, but adding `fundamental-mode' here fixes the issue.
  (setq git-gutter:disabled-modes '(fundamental-mode image-mode pdf-view-mode))
  :config
  (set-popup-rule! "^\\*git-gutter" :select nil :size '+popup-shrink-to-fit)

  ;; Only enable the backends that are available, so it doesn't have to check
  ;; when opening each buffer.
  (setq git-gutter:handled-backends
        (cons 'git (cl-remove-if-not #'executable-find (list 'hg 'svn 'bzr)
                                     :key #'symbol-name)))

  ;; Update git-gutter on focus (in case I was using git externally)
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
  ;; update git-gutter when using magit commands
  (advice-add #'magit-stage-file   :after #'+vc-gutter-update-h)
  (advice-add #'magit-unstage-file :after #'+vc-gutter-update-h)

  (defadvice! +vc-gutter--fix-linearity-of-hunks-a (diffinfos is-reverse)
    "Fixes `git-gutter:next-hunk' and `git-gutter:previous-hunk' sometimes
  jumping to random hunks."
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
  (when +vc-gutter-default-style
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
      nil nil 'bottom)))

;;;{{{2 drag-stuff
(use-package! drag-stuff
  :defer t
  :init
  (map! "<M-up>"    #'drag-stuff-up
        "<M-down>"  #'drag-stuff-down
        "<M-left>"  #'drag-stuff-left
        "<M-right>" #'drag-stuff-right))
;;;}}}
;;;}}}
;;;{{{1 after
(after! electric
  (setq-default electric-indent-chars '(?\n ?\^?))

  (add-hook! 'electric-indent-functions
    (defun +electric-indent-char-fn (_c)
      (when (and (eolp) +electric-indent-words)
        (save-excursion
          (backward-word)
          (looking-at-p (concat "\\<" (regexp-opt +electric-indent-words))))))))

(after! epa
  ;; With GPG 2.1+, this forces gpg-agent to use the Emacs minibuffer to prompt
  ;; for the key passphrase.
  (set 'epg-pinentry-mode 'loopback)
  ;; Default to the first enabled and non-expired key in your keyring.
  (setq-default
   epa-file-encrypt-to
   (or (default-value 'epa-file-encrypt-to)
       (unless (string-empty-p user-full-name)
         (when-let (context (ignore-errors (epg-make-context)))
           (cl-loop for key in (epg-list-keys context user-full-name 'public)
                    for subkey = (car (epg-key-sub-key-list key))
                    if (not (memq 'disabled (epg-sub-key-capability subkey)))
                    if (< (or (epg-sub-key-expiration-time subkey) 0)
                          (time-to-seconds))
                    collect (epg-sub-key-fingerprint subkey))))
       user-mail-address))
   ;; And suppress prompts if epa-file-encrypt-to has a default value (without
   ;; overwriting file-local values).
  (defadvice! +default--dont-prompt-for-keys-a (&rest _)
    :before #'epa-file-write-region
    (unless (local-variable-p 'epa-file-encrypt-to)
      (setq-local epa-file-encrypt-to (default-value 'epa-file-encrypt-to)))))

(after! woman
  ;; The woman-manpath default value does not necessarily match man. If we have
  ;; man available but aren't using it for performance reasons, we can extract
  ;; it's manpath.
  (when (executable-find "man")
    (setq woman-manpath
          (split-string (cdr (doom-call-process "man" "--path"))
                        path-separator t))))

  ;; You can disable :unless predicates with (sp-pair "'" nil :unless nil)
  ;; And disable :post-handlers with (sp-pair "{" nil :post-handlers nil)
  ;; or specific :post-handlers with:
  ;;   (sp-pair "{" nil :post-handlers '(:rem ("| " "SPC")))
(after! smartparens
  ;; Smartparens' navigation feature is neat, but does not justify how
  ;; expensive it is. It's also less useful for evil users. This may need to
  ;; be reactivated for non-evil users though. Needs more testing!
  (add-hook! 'after-change-major-mode-hook
    (defun doom-disable-smartparens-navigate-skip-match-h ()
      (setq sp-navigate-skip-match nil
            sp-navigate-consider-sgml-tags nil)))

  ;; Autopair quotes more conservatively; if I'm next to a word/before another
  ;; quote, I don't want to open a new pair or it would unbalance them.
  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p)))
    (sp-pair "'"  nil :unless unless-list)
    (sp-pair "\"" nil :unless unless-list))

  ;; Expand {|} => { | }
  ;; Expand {|} => {
  ;;   |
  ;; }
  (dolist (brace '("(" "{" "["))
    (sp-pair brace nil
             :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))
             ;; Don't autopair opening braces if before a word character or
             ;; other opening brace. The rationale: it interferes with manual
             ;; balancing of braces, and is odd form to have s-exps with no
             ;; whitespace in between, e.g. ()()(). Insert whitespace if
             ;; genuinely want to start a new form in the middle of a word.
             :unless '(sp-point-before-word-p sp-point-before-same-p)))

  ;; In lisps ( should open a new form if before another parenthesis
  (sp-local-pair sp-lisp-modes "(" ")" :unless '(:rem sp-point-before-same-p))

  ;; Major-mode specific fixes
  (sp-local-pair 'ruby-mode "{" "}"
                 :pre-handlers '(:rem sp-ruby-pre-handler)
                 :post-handlers '(:rem sp-ruby-post-handler))

  ;; Don't eagerly escape Swift style string interpolation
  (sp-local-pair 'swift-mode "\\(" ")" :when '(sp-in-string-p))

  ;; Don't do square-bracket space-expansion where it doesn't make sense to
  (sp-local-pair '(emacs-lisp-mode org-mode markdown-mode gfm-mode)
                 "[" nil :post-handlers '(:rem ("| " "SPC")))

  ;; Reasonable default pairs for HTML-style comments
  (sp-local-pair (append sp--html-modes '(markdown-mode gfm-mode))
                 "<!--" "-->"
                 :unless '(sp-point-before-word-p sp-point-before-same-p)
                 :actions '(insert) :post-handlers '(("| " "SPC")))

  ;; Disable electric keys in C modes because it interferes with smartparens
  ;; and custom bindings. We'll do it ourselves (mostly).
  (after! cc-mode
    (setq-default c-electric-flag nil)
    (dolist (key '("#" "{" "}" "/" "*" ";" "," ":" "(" ")" "\177"))
      (define-key c-mode-base-map key nil))

    ;; Smartparens and cc-mode both try to autoclose angle-brackets
    ;; intelligently. The result isn't very intelligent (causes redundant
    ;; characters), so just do it ourselves.
    (define-key! c++-mode-map "<" nil ">" nil)

    (defun +default-cc-sp-point-is-template-p (id action context)
      "Return t if point is in the right place for C++ angle-brackets."
      (and (sp-in-code-p id action context)
           (cond ((eq action 'insert)
                  (sp-point-after-word-p id action context))
                 ((eq action 'autoskip)
                  (/= (char-before) 32)))))

    (defun +default-cc-sp-point-after-include-p (id action context)
      "Return t if point is in an #include."
      (and (sp-in-code-p id action context)
           (save-excursion
             (goto-char (line-beginning-position))
             (looking-at-p "[ 	]*#include[^<]+"))))

    ;; ...and leave it to smartparens
    (sp-local-pair '(c++-mode objc-mode)
                   "<" ">"
                   :when '(+default-cc-sp-point-is-template-p
                           +default-cc-sp-point-after-include-p)
                   :post-handlers '(("| " "SPC")))

    (sp-local-pair '(c-mode c++-mode objc-mode java-mode)
                   "/*!" "*/"
                   :post-handlers '(("||\n[i]" "RET") ("[d-1]< | " "SPC"))))

  ;; Expand C-style comment blocks.
  (defun +default-open-doc-comments-block (&rest _ignored)
    (save-excursion
      (newline)
      (indent-according-to-mode)))
  (sp-local-pair
   '(js2-mode typescript-mode rjsx-mode rust-mode c-mode c++-mode objc-mode
              csharp-mode java-mode php-mode css-mode scss-mode less-css-mode
              stylus-mode scala-mode)
   "/*" "*/"
   :actions '(insert)
   :post-handlers '(("| " "SPC")
                    (" | " "*")
                    ("|[i]\n[i]" "RET")))

  (after! smartparens-ml
    (sp-with-modes '(tuareg-mode fsharp-mode)
      (sp-local-pair "(*" "*)" :actions nil)
      (sp-local-pair "(*" "*"
                     :actions '(insert)
                     :post-handlers '(("| " "SPC") ("|[i]*)[d-2]" "RET")))))

  (after! smartparens-markdown
    (sp-with-modes '(markdown-mode gfm-mode)
      (sp-local-pair "```" "```" :post-handlers '(:add ("||\n[i]" "RET")))

      ;; The original rules for smartparens had an odd quirk: inserting two
      ;; asterixex would replace nearby quotes with asterixes. These two rules
      ;; set out to fix this.
      (sp-local-pair "**" nil :actions :rem)
      (sp-local-pair "*" "*"
                     :actions '(insert skip)
                     :unless '(:rem sp-point-at-bol-p)
                     ;; * then SPC will delete the second asterix and assume
                     ;; you wanted a bullet point. * followed by another *
                     ;; will produce an extra, assuming you wanted **|**.
                     :post-handlers '(("[d1]" "SPC") ("|*" "*"))))

    ;; This keybind allows * to skip over **.
    (map! :map markdown-mode-map
          :ig "*" (general-predicate-dispatch nil
                    (looking-at-p "\\*\\* *")
                    (cmd! (forward-char 2)))))

  ;; Removes haskell-mode trailing braces
  (after! smartparens-haskell
    (sp-with-modes '(haskell-mode haskell-interactive-mode)
      (sp-local-pair "{-" "-}" :actions :rem)
      (sp-local-pair "{-#" "#-}" :actions :rem)
      (sp-local-pair "{-@" "@-}" :actions :rem)
      (sp-local-pair "{-" "-")
      (sp-local-pair "{-#" "#-")
      (sp-local-pair "{-@" "@-")))

  (after! smartparens-python
    (sp-with-modes 'python-mode
      ;; Automatically close f-strings
      (sp-local-pair "f\"" "\"")
      (sp-local-pair "f\"\"\"" "\"\"\"")
      (sp-local-pair "f'''" "'''")
      (sp-local-pair "f'" "'"))
    ;; Original keybind interferes with smartparens rules
    (define-key python-mode-map (kbd "DEL") nil)
    ;; Interferes with the def snippet in doom-snippets
    ;; TODO Fix this upstream, in doom-snippets, instead
    (setq sp-python-insert-colon-in-function-definitions nil)))

(after! which-key
  (let ((prefix-re (regexp-opt (list doom-leader-key doom-leader-alt-key))))
    (cl-pushnew `((,(format "\\`\\(?:<\\(?:\\(?:f1\\|help\\)>\\)\\|C-h\\|%s h\\) d\\'" prefix-re))
                  nil . "doom")
                which-key-replacement-alist)
    (cl-pushnew `((,(format "\\`\\(?:<\\(?:\\(?:f1\\|help\\)>\\)\\|C-h\\|%s h\\) r\\'" prefix-re))
                  nil . "reload")
                which-key-replacement-alist)
    (cl-pushnew `((,(format "\\`\\(?:<\\(?:\\(?:f1\\|help\\)>\\)\\|C-h\\|%s h\\) b\\'" prefix-re))
                  nil . "bindings")
                which-key-replacement-alist)))

;; Consistently use q to quit windows
(after! tabulated-list
  (define-key tabulated-list-mode-map "q" #'quit-window))
;;;}}}
;;;{{{1 advice

;; Highjacks backspace to delete up to nearest column multiple of `tab-width' at
;; a time. If you have smartparens enabled, it will also:
;;  a) balance spaces inside brackets/parentheses ( | ) -> (|)
;;  b) close empty multiline brace blocks in one step:
;;     {
;;     |
;;     }
;;     becomes {|}
;;  c) refresh smartparens' :post-handlers, so SPC and RET expansions work even
;;     after a backspace.
;;  d) properly delete smartparen pairs when they are encountered, without the
;;     need for strict mode.
;;  e) do none of this when inside a string
(advice-add #'delete-backward-char :override #'+default--delete-backward-char-a)

;; HACK Makes `newline-and-indent' continue comments (and more reliably).
;;      Consults `doom-point-in-comment-functions' to detect a commented region
;;      and uses that mode's `comment-line-break-function' to continue comments.
;;      If neither exists, it will fall back to the normal behavior of
;;      `newline-and-indent'.
;;
;;      We use an advice here instead of a remapping because many modes define
;;      and remap to their own newline-and-indent commands, and tackling all
;;      those cases was judged to be more work than dealing with the edge cases
;;      on a case by case basis.
(defadvice! +default--newline-indent-and-continue-comments-a (&rest _)
  "A replacement for `newline-and-indent'.

Continues comments if executed from a commented line. Consults
`doom-point-in-comment-functions' to determine if in a comment."
  :before-until #'newline-and-indent
  (interactive "*")
  (when (and +default-want-RET-continue-comments
             (doom-point-in-comment-p)
             (functionp comment-line-break-function))
    (funcall comment-line-break-function nil)
    t))

;;;}}}

(load! "+bindings")

;;; vim:filetype=lisp foldmethod=marker
