;;; config/default/+bindings.el -*- lexical-binding: t; -*-

(when (featurep! :editor evil +everywhere)
  ;; NOTE SPC u replaces C-u as the universal argument.

  ;; Minibuffer
  (map! :map (evil-ex-completion-map evil-ex-search-keymap)
        "C-a" #'evil-beginning-of-line
        "C-b" #'evil-backward-char
        "C-f" #'evil-forward-char
        :gi "C-j" #'next-complete-history-element
        :gi "C-k" #'previous-complete-history-element)

  (define-key! :keymaps +default-minibuffer-maps
    [escape] #'abort-recursive-edit
    "C-a"    #'move-beginning-of-line
    "C-r"    #'evil-paste-from-register
    "C-u"    #'evil-delete-back-to-indentation
    "C-v"    #'yank
    "C-w"    #'doom/delete-backward-word
    "C-z"    (cmd! (ignore-errors (call-interactively #'undo))))

  (define-key! :keymaps +default-minibuffer-maps
    "C-j"    #'next-line
    "C-k"    #'previous-line
    "C-S-j"  #'scroll-up-command
    "C-S-k"  #'scroll-down-command)
  ;; For folks with `evil-collection-setup-minibuffer' enabled
  (define-key! :states 'insert :keymaps +default-minibuffer-maps
    "C-j"    #'next-line
    "C-k"    #'previous-line)
  (define-key! read-expression-map
    "C-j" #'next-line-or-history-element
    "C-k" #'previous-line-or-history-element))


;;
;;; Global keybindings

;; Smart tab, these will only work in GUI Emacs
(map! :i [tab] (cmds! (and (featurep! :editor snippets)
                           (yas-maybe-expand-abbrev-key-filter 'yas-expand))
                      #'yas-expand
                      (and (bound-and-true-p company-mode)
                           (featurep! :completion company +tng))
                      #'company-indent-or-complete-common)
      :m [tab] (cmds! (and (featurep! :editor snippets)
                           (evil-visual-state-p)
                           (or (eq evil-visual-selection 'line)
                               (not (memq (char-after) (list ?\( ?\[ ?\{ ?\} ?\] ?\))))))
                      #'yas-insert-snippet
                      (and (featurep! :editor fold)
                           (save-excursion (end-of-line) (invisible-p (point))))
                      #'+fold/toggle
                      ;; Fixes #4548: without this, this tab keybind overrides
                      ;; mode-local ones for modes that don't have an evil
                      ;; keybinding scheme or users who don't have :editor (evil
                      ;; +everywhere) enabled.
                      (or (doom-lookup-key
                           [tab]
                           (list (evil-get-auxiliary-keymap (current-local-map) evil-state)
                                 (current-local-map)))
                          (doom-lookup-key
                           (kbd "TAB")
                           (list (evil-get-auxiliary-keymap (current-local-map) evil-state)))
                          (doom-lookup-key (kbd "TAB") (list (current-local-map))))
                      it
                      (fboundp 'evil-jump-item)
                      #'evil-jump-item)

      (:after help :map help-mode-map
       :n "o"       #'link-hint-open-link)
      (:after helpful :map helpful-mode-map
       :n "o"       #'link-hint-open-link)
      (:after info :map Info-mode-map
       :n "o"       #'link-hint-open-link)
      (:after apropos :map apropos-mode-map
       :n "o"       #'link-hint-open-link
       :n "TAB"     #'forward-button
       :n [tab]     #'forward-button
       :n [backtab] #'backward-button)
      (:after view :map view-mode-map
       [escape]  #'View-quit-all)
      (:after man :map Man-mode-map
       :n "q"    #'kill-current-buffer)
      (:after geiser-doc :map geiser-doc-mode-map
       :n "o"    #'link-hint-open-link)

      (:unless (featurep! :input layout +bepo)
        (:after (evil-org evil-easymotion)
         :map evil-org-mode-map
         :m "gsh" #'+org/goto-visible))

      (:when (featurep! :editor multiple-cursors)
       :prefix "gz"
       :nv "d" #'evil-mc-make-and-goto-next-match
       :nv "D" #'evil-mc-make-and-goto-prev-match
       :nv "s" #'evil-mc-skip-and-goto-next-match
       :nv "S" #'evil-mc-skip-and-goto-prev-match
       :nv "c" #'evil-mc-skip-and-goto-next-cursor
       :nv "C" #'evil-mc-skip-and-goto-prev-cursor
       :nv "j" #'evil-mc-make-cursor-move-next-line
       :nv "k" #'evil-mc-make-cursor-move-prev-line
       :nv "m" #'evil-mc-make-all-cursors
       :nv "n" #'evil-mc-make-and-goto-next-cursor
       :nv "N" #'evil-mc-make-and-goto-last-cursor
       :nv "p" #'evil-mc-make-and-goto-prev-cursor
       :nv "P" #'evil-mc-make-and-goto-first-cursor
       :nv "q" #'evil-mc-undo-all-cursors
       :nv "t" #'+multiple-cursors/evil-mc-toggle-cursors
       :nv "u" #'+multiple-cursors/evil-mc-undo-cursor
       :nv "z" #'+multiple-cursors/evil-mc-toggle-cursor-here
       :v  "I" #'evil-mc-make-cursor-in-visual-selection-beg
       :v  "A" #'evil-mc-make-cursor-in-visual-selection-end)

      ;; misc
      :n "C-S-f"  #'toggle-frame-fullscreen
      :n "C-+"    #'doom/reset-font-size
      ;; Buffer-local font resizing
      :n "C-="    #'text-scale-increase
      :n "C--"    #'text-scale-decrease
      ;; Frame-local font resizing
      :n "M-C-="  #'doom/increase-font-size
      :n "M-C--"  #'doom/decrease-font-size)

(map! :nmvo ","        nil
      :gni  "M-s"        #'save-buffer
      :n    "ga"         #'ff-find-other-file
      :n    "go"         (λ! (message "%S" (text-properties-at (point))))
      :gni  "C-j"        #'ace-window
      :i    "C-f"        #'forward-char
      :i    "C-b"        #'backward-char
      :n    "C-t"        #'pop-tag-mark
      ;; vterm
      :ni   "M-t"       #'+vterm/toggle
      :ni   [f5]        #'+vterm/toggle)

;;
;;; Module keybinds

;;; :completion
(map! (:when (featurep! :completion company)
       :i "C-@"    (cmds! (not (minibufferp)) #'company-complete-common)
       :i "C-SPC"  (cmds! (not (minibufferp)) #'company-complete-common)
       :i [C-tab]  #'+company/complete
       (:prefix "C-x"
        :i "e"      #'company-english-helper-search
        :i "t"      #'company-tabnine)
       (:after company
        (:map company-active-map
         "C-w"     nil  ; don't interfere with `evil-delete-backward-word'
         "C-n"     #'company-select-next
         "C-p"     #'company-select-previous
         "C-j"     #'company-select-next
         "C-k"     #'company-select-previous
         "C-h"     #'company-show-doc-buffer
         "C-u"     #'company-previous-page
         "C-d"     #'company-next-page
         "C-s"     #'company-filter-candidates
         "C-S-s"   (cond ((featurep! :completion vertico)  #'completion-at-point)
                         ((featurep! :completion ivy)      #'counsel-company)
                         ((featurep! :completion helm)     #'helm-company))
         "C-SPC"   #'company-complete-common
         "TAB"     #'+company/smarter-yas-expand-next-field-complete
         [tab]     #'+company/smarter-yas-expand-next-field-complete
         [C-return] #'+company/return-cancel-completion
         [backtab] #'company-select-previous
         [f1]      nil)
        (:map company-search-map  ; applies to `company-filter-map' too
         "C-n"     #'company-select-next-or-abort
         "C-p"     #'company-select-previous-or-abort
         "C-j"     #'company-select-next-or-abort
         "C-k"     #'company-select-previous-or-abort
         "C-s"     #'company-filter-candidates
         [escape]  #'company-search-abort)))

      (:when (featurep! :completion ivy)
       (:after ivy
        :map ivy-minibuffer-map
        "C-SPC" #'ivy-call-and-recenter  ; preview file
        "C-l"   #'ivy-alt-done
        "C-v"   #'yank)
       (:after counsel
        :map counsel-ag-map
        "C-SPC"    #'ivy-call-and-recenter ; preview
        "C-l"      #'ivy-done
        [C-return] #'+ivy/git-grep-other-window-action))

      (:when (featurep! :completion helm)
       (:after helm :map helm-map
        [remap next-line]     #'helm-next-line
        [remap previous-line] #'helm-previous-line
        [left]     #'left-char
        [right]    #'right-char
        "C-S-f"    #'helm-previous-page
        "C-S-n"    #'helm-next-source
        "C-S-p"    #'helm-previous-source
        (:when (featurep! :editor evil +everywhere)
         "C-j"    #'helm-next-line
         "C-k"    #'helm-previous-line
         "C-S-j"  #'helm-next-source
         "C-S-k"  #'helm-previous-source)
        "C-u"      #'helm-delete-minibuffer-contents
        "C-s"      #'helm-minibuffer-history
        ;; Swap TAB and C-z
        "TAB"      #'helm-execute-persistent-action
        [tab]      #'helm-execute-persistent-action
        "C-z"      #'helm-select-action)
       (:after helm-ag :map helm-ag-map
        "C--"      #'+helm-do-ag-decrease-context
        "C-="      #'+helm-do-ag-increase-context
        [left]     nil
        [right]    nil)
       (:after helm-files :map (helm-find-files-map helm-read-file-map)
        [C-return] #'helm-ff-run-switch-other-window
        "C-w"      #'helm-find-files-up-one-level)
       (:after helm-locate :map helm-generic-files-map
        [C-return] #'helm-ff-run-switch-other-window)
       (:after helm-buffers :map helm-buffer-map
        [C-return] #'helm-buffer-switch-other-window)
       (:after helm-occur :map helm-occur-map
        [C-return] #'helm-occur-run-goto-line-ow)
       (:after helm-grep :map helm-grep-map
        [C-return] #'helm-grep-run-other-window-action))

      (:when (featurep! :completion vertico)
        (:after vertico
         :map vertico-map
         "M-RET" #'vertico-exit-input
         "C-SPC" #'+vertico/embark-preview
         "C-j"   #'vertico-next
         "C-M-j" #'vertico-next-group
         "C-k"   #'vertico-previous
         "C-M-k" #'vertico-previous-group)))


;;; :editor
(map! (:when (featurep! :editor popup)
       "C-`"   #'+popup/toggle
       "C-~"   #'+popup/raise
       "C-x p" #'+popup/other)

      (:when (featurep! :editor workspaces)
       :n "C-S-t" #'+workspace/display
       :g "M-1"   #'+workspace/switch-to-0
       :g "M-2"   #'+workspace/switch-to-1
       :g "M-3"   #'+workspace/switch-to-2
       :g "M-4"   #'+workspace/switch-to-3
       :g "M-5"   #'+workspace/switch-to-4
       :g "M-6"   #'+workspace/switch-to-5
       :g "M-7"   #'+workspace/switch-to-6
       :g "M-8"   #'+workspace/switch-to-7
       :g "M-9"   #'+workspace/switch-to-8
       :g "M-0"   #'+workspace/switch-to-final
       (:when IS-MAC
        :g "s-t"   #'+workspace/new
        :g "s-T"   #'+workspace/display
        :n "s-1"   #'+workspace/switch-to-0
        :n "s-2"   #'+workspace/switch-to-1
        :n "s-3"   #'+workspace/switch-to-2
        :n "s-4"   #'+workspace/switch-to-3
        :n "s-5"   #'+workspace/switch-to-4
        :n "s-6"   #'+workspace/switch-to-5
        :n "s-7"   #'+workspace/switch-to-6
        :n "s-8"   #'+workspace/switch-to-7
        :n "s-9"   #'+workspace/switch-to-8
        :n "s-0"   #'+workspace/switch-to-final))
      (:when (featurep! :editor tabs)
       :ni "M-j"
       (cond ((featurep! :editor tabs +sort)    nil)
             ((featurep! :editor tabs)          #'awesome-tab-ace-jump))
       :ni "M-h"
       (cond ((featurep! :editor tabs +sort)    #'sort-tab-select-prev-tab)
             ((featurep! :editor tabs)          #'awesome-tab-backward-tab))
       :ni "M-l"
       (cond ((featurep! :editor tabs +sort)    #'sort-tab-select-next-tab)
             ((featurep! :editor tabs)          #'awesome-tab-forward-tab))
       (:after org
        :map org-mode-map
        :ni "M-j"
        (cond ((featurep! :editor tabs +sort)    nil)
              ((featurep! :editor tabs)          #'awesome-tab-ace-jump))
        :ni "M-h"
        (cond ((featurep! :editor tabs +sort)    #'sort-tab-select-prev-tab)
              ((featurep! :editor tabs)          #'awesome-tab-backward-tab))
        :ni "M-l"
        (cond ((featurep! :editor tabs +sort)    #'sort-tab-select-next-tab)
              ((featurep! :editor tabs)          #'awesome-tab-forward-tab)))
       (:after evil-org
        :map evil-org-mode-map
        :ni "M-j"
        (cond ((featurep! :editor tabs +sort)    nil)
              ((featurep! :editor tabs)          #'awesome-tab-ace-jump))
        :ni "M-h"
        (cond ((featurep! :editor tabs +sort)    #'sort-tab-select-prev-tab)
              ((featurep! :editor tabs)          #'awesome-tab-backward-tab))
        :ni "M-l"
        (cond ((featurep! :editor tabs +sort)    #'sort-tab-select-next-tab)
              ((featurep! :editor tabs)          #'awesome-tab-forward-tab)))
       (:after evil-markdown
        :map evil-markdown-mode-map
        :ni "M-j"
        (cond ((featurep! :editor tabs +sort)    nil)
              ((featurep! :editor tabs)          #'awesome-tab-ace-jump))
        :ni "M-h"
        (cond ((featurep! :editor tabs +sort)    #'sort-tab-select-prev-tab)
              ((featurep! :editor tabs)          #'awesome-tab-backward-tab))
        :ni "M-l"
        (cond ((featurep! :editor tabs +sort)    #'sort-tab-select-next-tab)
              ((featurep! :editor tabs)          #'awesome-tab-forward-tab)))
       (:after info
        :map Info-mode-map
        :ni "C-j"       #'ace-window)))

;;; :editor
(map! (:when (featurep! :editor format)
       :n "gQ" #'+format:region)

      (:when (featurep! :editor rotate-text)
       :n "]r"  #'rotate-text
       :n "[r"  #'rotate-text-backward)

      (:when (featurep! :editor multiple-cursors)
       ;; evil-multiedit
       :v  "R"     #'evil-multiedit-match-all
       :n  "M-d"   #'evil-multiedit-match-symbol-and-next
       :n  "M-D"   #'evil-multiedit-match-symbol-and-prev
       :v  "M-d"   #'evil-multiedit-match-and-next
       :v  "M-D"   #'evil-multiedit-match-and-prev
       :nv "C-M-d" #'evil-multiedit-restore
       (:after evil-multiedit
        (:map evil-multiedit-mode-map
         :nv "M-d" #'evil-multiedit-match-and-next
         :nv "M-D" #'evil-multiedit-match-and-prev
         [return]  #'evil-multiedit-toggle-or-restrict-region))))

;;; :tools
(when (featurep! :tools eval)
  (map! "M-r" #'+eval/buffer))

;;; :avy-thins-edit
(map! :nmvo ","         nil
      (:prefix-map ("C-c y" . "avy-copy-and-yank")
       :ni "w"      #'avy-thing-copy-and-yank-word
       :ni "o"      #'avy-thing-copy-and-yank-symbol
       :ni "x"      #'avy-thing-copy-and-yank-sexp
       :ni "l"      #'avy-thing-copy-and-yank-line
       :ni "b"      #'avy-thing-copy-and-yank-parentheses
       :ni "("      #'avy-thing-copy-and-yank-parentheses
       :ni "p"      #'avy-thing-copy-and-yank-paragraph
       :ni "{"      #'avy-thing-copy-and-yank-paragraph
       :ni "n"      #'avy-thing-copy-and-yank-number
       :ni "f"      #'avy-thing-copy-and-yank-defun
       :ni "e"      #'avy-thing-copy-and-yank-email
       :ni "i"      #'avy-thing-copy-and-yank-filename
       :ni "t"      #'avy-thing-copy-and-yank-list
       :ni "u"      #'avy-thing-copy-and-yank-url)
      (:prefix-map ("C-c j" . "avy-thing-edit")
       (:prefix-map ("c" . "copy")
        :ni "w"      #'avy-thing-copy-word
        :ni "o"      #'avy-thing-copy-symbol
        :ni "x"      #'avy-thing-copy-sexp
        :ni "l"      #'avy-thing-copy-line
        :ni "b"      #'avy-thing-copy-parentheses
        :ni "("      #'avy-thing-copy-parentheses
        :ni "p"      #'avy-thing-copy-paragraph
        :ni "{"      #'avy-thing-copy-paragraph
        :ni "n"      #'avy-thing-copy-number
        :ni "f"      #'avy-thing-copy-defun
        :ni "e"      #'avy-thing-copy-email
        :ni "i"      #'avy-thing-copy-filename
        :ni "t"      #'avy-thing-copy-list
        :ni "u"      #'avy-thing-copy-url)
       (:prefix-map ("y" . "copy and yank")
        :ni "w"      #'avy-thing-copy-and-yank-word
        :ni "o"      #'avy-thing-copy-and-yank-symbol
        :ni "x"      #'avy-thing-copy-and-yank-sexp
        :ni "l"      #'avy-thing-copy-and-yank-line
        :ni "b"      #'avy-thing-copy-and-yank-parentheses
        :ni "("      #'avy-thing-copy-and-yank-parentheses
        :ni "p"      #'avy-thing-copy-and-yank-paragraph
        :ni "{"      #'avy-thing-copy-and-yank-paragraph
        :ni "n"      #'avy-thing-copy-and-yank-number
        :ni "f"      #'avy-thing-copy-and-yank-defun
        :ni "e"      #'avy-thing-copy-and-yank-email
        :ni "i"      #'avy-thing-copy-and-yank-filename
        :ni "t"      #'avy-thing-copy-and-yank-list
        :ni "u"      #'avy-thing-copy-and-yank-url)
       (:prefix-map ("x" . "cut")
        :ni "w"      #'avy-thing-cut-word
        :ni "o"      #'avy-thing-cut-symbol
        :ni "x"      #'avy-thing-cut-sexp
        :ni "l"      #'avy-thing-cut-line
        :ni "b"      #'avy-thing-cut-parentheses
        :ni "("      #'avy-thing-cut-parentheses
        :ni "p"      #'avy-thing-cut-paragraph
        :ni "{"      #'avy-thing-cut-paragraph
        :ni "n"      #'avy-thing-cut-number
        :ni "f"      #'avy-thing-cut-defun
        :ni "e"      #'avy-thing-cut-email
        :ni "i"      #'avy-thing-cut-filename
        :ni "t"      #'avy-thing-cut-list
        :ni "u"      #'avy-thing-cut-url)
       (:prefix-map ("r" . "replace")
        :ni "w"      #'avy-thing-replace-word
        :ni "o"      #'avy-thing-replace-symbol
        :ni "x"      #'avy-thing-replace-sexp
        :ni "l"      #'avy-thing-replace-line
        :ni "b"      #'avy-thing-replace-parentheses
        :ni "("      #'avy-thing-replace-parentheses
        :ni "p"      #'avy-thing-replace-paragraph
        :ni "{"      #'avy-thing-replace-paragraph
        :ni "n"      #'avy-thing-replace-number
        :ni "f"      #'avy-thing-replace-defun
        :ni "e"      #'avy-thing-replace-email
        :ni "i"      #'avy-thing-replace-filename
        :ni "t"      #'avy-thing-replace-list
        :ni "u"      #'avy-thing-replace-url)))

;;
;;; <leader>

(map! :leader
      :desc "Eval expression"       ";"    #'pp-eval-expression
      :desc "M-x"                   ":"    #'execute-extended-command
      :desc "Pop up scratch buffer" "X"    #'doom/open-scratch-buffer
      :desc "Org Capture"           "x"    #'org-capture
      ;; C-u is used by evil
      :desc "Universal argument"    "u"    #'universal-argument
      :desc "window"                "w"    evil-window-map
      :desc "help"                  "h"    help-map

      (:when (featurep! :editor popup)
       :desc "Toggle last popup"     "~"    #'+popup/toggle)
      :desc "Find file"             "."    #'find-file
      :desc "Switch buffer"         ","    #'switch-to-buffer
      (:when (featurep! :editor workspaces)
       :desc "Switch workspace buffer" "," #'persp-switch-to-buffer
       :desc "Switch buffer"           "<" #'switch-to-buffer)
      :desc "Switch to last buffer" "`"    #'evil-switch-to-windows-last-buffer
      :desc "Resume last search"    "r"
      (cond ((featurep! :completion vertico)    #'vertico-repeat)
            ((featurep! :completion ivy)        #'ivy-resume)
            ((featurep! :completion helm)       #'helm-resume))
      :desc "Run terminal"          "'"    #'vterm

      :desc "Search for symbol in project" "*" #'+default/search-project-for-symbol-at-point
      :desc "Search project"               "/" #'+default/search-project

      :desc "M-x"                   "SPC"  #'execute-extended-command
      :desc "Jump to bookmark"      "RET"  #'bookmark-jump
      :desc "Alternate buffer"      "TAB"  #'+default/alternate-buffer-in-persp
      :desc "Find everything"       "a"    #'snail
      (:when (featurep! :editor search +snails)
       :desc "Find everything"      "a"    #'snail)

      ;;; <leader> b --- buffer
      (:prefix-map ("b" . "buffer")
       :desc "Alternate buffer"           "TAB"  #'+default/alternate-buffer-in-persp
       :desc "Toggle narrowing"            "-"   #'doom/toggle-narrow-buffer
       :desc "Previous buffer"             "["   #'previous-buffer
       :desc "Next buffer"                 "]"   #'next-buffer
       :desc "Switch buffer"               "b"   #'switch-to-buffer
       (:when (featurep! :editor workspaces)
        :desc "Switch buffer"              "B"   #'switch-to-buffer)
       :desc "Clone buffer"                "c"   #'clone-indirect-buffer
       :desc "Clone buffer other window"   "C"   #'clone-indirect-buffer-other-window
       :desc "Kill buffer"                 "d"   #'kill-current-buffer
       (:when (featurep! :editor tabs +sort)
         :desc "Kill buffer"               "d"   #'sort-tab-close-current-tab)
       :desc "ibuffer"                     "i"   #'ibuffer
       :desc "Kill buffer"                 "k"   #'kill-current-buffer
       :desc "Kill all buffers"            "K"   #'doom/kill-all-buffers
       :desc "Switch to last buffer"       "l"   #'evil-switch-to-windows-last-buffer
       :desc "Set bookmark"                "m"   #'bookmark-set
       :desc "Delete bookmark"             "M"   #'bookmark-delete
       :desc "Next buffer"                 "n"   #'next-buffer
       :desc "New empty buffer"            "N"   #'evil-buffer-new
       :desc "Kill other buffers"          "O"   #'doom/kill-other-buffers
       :desc "Previous buffer"             "p"   #'previous-buffer
       :desc "Revert buffer"               "r"   #'revert-buffer
       :desc "Save buffer"                 "s"   #'basic-save-buffer
       :desc "Save all buffers"            "S"   #'evil-write-all
       :desc "Save buffer as root"         "u"   #'doom/sudo-save-buffer
       :desc "Pop up scratch buffer"       "x"   #'doom/open-scratch-buffer
       :desc "Switch to scratch buffer"    "X"   #'doom/switch-to-scratch-buffer
       :desc "Bury buffer"                 "z"   #'bury-buffer
       :desc "Kill buried buffers"         "Z"   #'doom/kill-buried-buffers)

      ;;; <leader> c --- code
      (:prefix-map ("c" . "code")
       (:when (and (featurep! :tools lsp) (not (featurep! :tools lsp +eglot)))
        :desc "LSP Execute code action" "a" #'lsp-execute-code-action
        :desc "LSP Organize imports" "o" #'lsp-organize-imports
        (:when (featurep! :completion ivy)
         :desc "Jump to symbol in current workspace" "j"   #'lsp-ivy-workspace-symbol
         :desc "Jump to symbol in any workspace"     "J"   #'lsp-ivy-global-workspace-symbol)
        (:when (featurep! :completion helm)
         :desc "Jump to symbol in current workspace" "j"   #'helm-lsp-workspace-symbol
         :desc "Jump to symbol in any workspace"     "J"   #'helm-lsp-global-workspace-symbol)
        (:when (featurep! :completion vertico)
         :desc "Jump to symbol in current workspace" "j"   #'consult-lsp-symbols
         :desc "Jump to symbol in any workspace"     "J"   (cmd!! #'consult-lsp-symbols 'all-workspaces))
        (:when (featurep! :editor treemacs +lsp)
         :desc "Errors list"                         "X"   #'lsp-treemacs-errors-list
         :desc "Incoming call hierarchy"             "y"   #'lsp-treemacs-call-hierarchy
         :desc "Outgoing call hierarchy"             "Y"   (cmd!! #'lsp-treemacs-call-hierarchy t)
         :desc "References tree"                     "R"   (cmd!! #'lsp-treemacs-references t)
         :desc "Symbols"                             "S"   #'lsp-treemacs-symbols)
        :desc "LSP"                                  "l"   #'+default/lsp-command-map
        :desc "LSP Rename"                           "r"   #'lsp-rename)
       (:when (featurep! :tools lsp +eglot)
        :desc "LSP Execute code action" "a" #'eglot-code-actions
        :desc "LSP Rename" "r" #'eglot-rename
        :desc "LSP Find declaration"                 "j"   #'eglot-find-declaration
        (:when (featurep! :completion vertico)
         :desc "Jump to symbol in current workspace" "j"   #'consult-eglot-symbols))
       :desc "Compile"                               "c"   #'compile
       :desc "Recompile"                             "C"   #'recompile
       :desc "Jump to definition"                    "d"   #'+lookup/definition
       :desc "Jump to references"                    "D"   #'+lookup/references
       :desc "Evaluate buffer/region"                "e"   #'+eval/buffer-or-region
       :desc "Evaluate & replace region"             "E"   #'+eval:replace-region
       :desc "Format buffer/region"                  "f"   #'+format/region-or-buffer
       :desc "Find implementations"                  "i"   #'+lookup/implementations
       :desc "Jump to documentation"                 "k"   #'+lookup/documentation
       :desc "Send to repl"                          "s"   #'+eval/send-region-to-repl
       :desc "Find type definition"                  "t"   #'+lookup/type-definition
       :desc "Delete trailing whitespace"            "w"   #'delete-trailing-whitespace
       :desc "Delete trailing newlines"              "W"   #'doom/delete-trailing-newlines
       :desc "List errors"                           "x"   #'+default/diagnostics)

      ;;; <leader> f --- file
      (:prefix-map ("f" . "file")
       :desc "Find file"                   "."   #'find-file
       :desc "Open project editorconfig"   "c"   #'editorconfig-find-current-editorconfig
       :desc "Copy this file"              "C"   #'doom/copy-this-file
       :desc "Find directory"              "d"   #'+default/dired
       :desc "Delete this file"            "D"   #'doom/delete-this-file
       :desc "Find file in emacs.d"        "e"   #'doom/find-file-in-emacsd
       :desc "Browse emacs.d"              "E"   #'doom/browse-in-emacsd
       :desc "Recursive find file"         "f"   #'+default/find-file-under-here
       :desc "Find file from here"         "F"   #'+default/find-file-under-here
       :desc "Locate file"                 "l"   #'locate
       :desc "Find file in private config" "p"   #'doom/find-file-in-private-config
       :desc "Browse private config"       "P"   #'doom/open-private-config
       :desc "Recent files"                "r"   #'recentf-open-files
       :desc "Rename/move file"            "R"   #'doom/move-this-file
       :desc "Save file"                   "s"   #'save-buffer
       :desc "Save file as..."             "S"   #'write-file
       :desc "Sudo find file"              "u"   #'doom/sudo-find-file
       :desc "Sudo this file"              "U"   #'doom/sudo-this-file
       :desc "Yank file path"              "y"   #'+default/yank-buffer-path
       :desc "Yank file path from project" "Y"   #'+default/yank-buffer-path-relative-to-project)

      ;;; <leader> g --- git/version control
      (:prefix-map ("g" . "git")
       :desc "Revert file"                 "R"   #'vc-revert
       :desc "Copy link to remote"         "y"   #'+vc/browse-at-remote-kill
       :desc "Copy link to homepage"       "Y"   #'+vc/browse-at-remote-kill-homepage
       (:when (featurep! :editor vc-gutter)
        :desc "Revert hunk"               "r"   #'git-gutter:revert-hunk
        :desc "Git stage hunk"            "s"   #'git-gutter:stage-hunk
        :desc "Git time machine"          "t"   #'git-timemachine-toggle
        :desc "Jump to next hunk"         "]"   #'git-gutter:next-hunk
        :desc "Jump to previous hunk"     "["   #'git-gutter:previous-hunk)
       (:when (featurep! :tools magit)
        :desc "Magit dispatch"            "/"   #'magit-dispatch
        :desc "Magit file dispatch"       "."   #'magit-file-dispatch
        :desc "Forge dispatch"            "'"   #'forge-dispatch
        :desc "Magit switch branch"       "b"   #'magit-branch-checkout
        :desc "Magit status"              "g"   #'magit-status
        :desc "Magit status here"         "G"   #'magit-status-here
        :desc "Magit file delete"         "D"   #'magit-file-delete
        :desc "Magit blame"               "B"   #'magit-blame-addition
        :desc "Magit clone"               "C"   #'magit-clone
        :desc "Magit fetch"               "F"   #'magit-fetch
        :desc "Magit buffer log"          "L"   #'magit-log-buffer-file
        :desc "Git stage file"            "S"   #'magit-stage-file
        :desc "Git unstage file"          "U"   #'magit-unstage-file
        (:prefix ("f" . "find")
         :desc "Find file"                 "f"   #'magit-find-file
         :desc "Find gitconfig file"       "g"   #'magit-find-git-config-file
         :desc "Find commit"               "c"   #'magit-show-commit
         :desc "Find issue"                "i"   #'forge-visit-issue
         :desc "Find pull request"         "p"   #'forge-visit-pullreq)
        (:prefix ("o" . "open in browser")
         :desc "Browse file or region"     "o"   #'+vc/browse-at-remote
         :desc "Browse homepage"           "h"   #'+vc/browse-at-remote-homepage
         :desc "Browse remote"             "r"   #'forge-browse-remote
         :desc "Browse commit"             "c"   #'forge-browse-commit
         :desc "Browse an issue"           "i"   #'forge-browse-issue
         :desc "Browse a pull request"     "p"   #'forge-browse-pullreq
         :desc "Browse issues"             "I"   #'forge-browse-issues
         :desc "Browse pull requests"      "P"   #'forge-browse-pullreqs)
        (:prefix ("l" . "list")
         :desc "List repositories"         "r"   #'magit-list-repositories
         :desc "List submodules"           "s"   #'magit-list-submodules
         :desc "List issues"               "i"   #'forge-list-issues
         :desc "List pull requests"        "p"   #'forge-list-pullreqs
         :desc "List notifications"        "n"   #'forge-list-notifications)
        (:prefix ("c" . "create")
         :desc "Initialize repo"           "r"   #'magit-init
         :desc "Clone repo"                "R"   #'magit-clone
         :desc "Commit"                    "c"   #'magit-commit-create
         :desc "Fixup"                     "f"   #'magit-commit-fixup
         :desc "Branch"                    "b"   #'magit-branch-and-checkout
         :desc "Issue"                     "i"   #'forge-create-issue
         :desc "Pull request"              "p"   #'forge-create-pullreq)))

      ;;; <leader> i --- insert
      (:prefix-map ("i" . "insert")
       :desc "Emoji"                         "e"   #'emojify-insert-emoji
       :desc "Current file name"             "f"   #'+default/insert-file-path
       :desc "Current file path"             "F"   (cmd!! #'+default/insert-file-path t)
       :desc "Evil ex path"                  "p"   (cmd! (evil-ex "R!echo "))
       :desc "From evil register"            "r"   #'evil-show-registers
       :desc "Snippet"                       "s"   #'yas-insert-snippet
       :desc "Unicode"                       "u"   #'insert-char
       :desc "From clipboard"                "y"   #'+default/yank-pop)

      ;;; <leader> j --- jump
      (:prefix-map ("j" . "jump")
       :desc "avy goto char timer"        "j"   #'evil-avy-goto-char-timer
       :desc "avy goto 2 char"            "c"   #'evil-avy-goto-char-2
       :desc "avy goto char"              "C"   #'evil-avy-goto-char
       :desc "avy goto line"              "l"   #'evil-avy-goto-line
       :desc "avy goto word"              "w"   #'evil-avy-goto-word-1
       :desc "avy goto symbol"            "o"   #'evil-avy-goto-symbol-1)

      ;;; <leader> k --- workspace
      (:when (featurep! :editor workspaces)
       (:prefix-map ("k" . "workspace")
        :desc "Display tab bar"           "TAB" #'+workspace/display
        :desc "Switch workspace"          "."   #'+workspace/switch-to
        :desc "Switch to last workspace"  "`"   #'+workspace/other
        :desc "New workspace"             "n"   #'+workspace/new
        :desc "New name workspace"        "N"   #'+workspace/new-named
        :desc "Load workspace from file"  "l"   #'+workspace/load
        :desc "Save workspace to file"    "s"   #'+workspace/save
        :desc "Delete session"            "x"   #'+workspace/kill-session
        :desc "Delete this workspace"     "d"   #'+workspace/delete
        :desc "Rename workspace"          "r"   #'+workspace/rename
        :desc "Restore last session"      "R"   #'+workspace/restore-last-session
        :desc "Next workspace"            "]"   #'+workspace/switch-right
        :desc "Previous workspace"        "["   #'+workspace/switch-left
        :desc "Switch to 1st workspace"   "1"   #'+workspace/switch-to-0
        :desc "Switch to 2nd workspace"   "2"   #'+workspace/switch-to-1
        :desc "Switch to 3rd workspace"   "3"   #'+workspace/switch-to-2
        :desc "Switch to 4th workspace"   "4"   #'+workspace/switch-to-3
        :desc "Switch to 5th workspace"   "5"   #'+workspace/switch-to-4
        :desc "Switch to 6th workspace"   "6"   #'+workspace/switch-to-5
        :desc "Switch to 7th workspace"   "7"   #'+workspace/switch-to-6
        :desc "Switch to 8th workspace"   "8"   #'+workspace/switch-to-7
        :desc "Switch to 9th workspace"   "9"   #'+workspace/switch-to-8
        :desc "Switch to final workspace" "0"   #'+workspace/switch-to-final))


      ;;; <leader> m --- mark
      (:prefix-map ("m" . "mark")
       :desc "Mark symbol highlight"      "m"   #'symbol-overlay-put
       :desc "Clear all highlight"        "c"   #'symbol-overlay-remove-all)

      ;;; <leader> n --- notes
      (:prefix-map ("n" . "notes")
       :desc "Search notes for symbol"      "*" #'+default/search-notes-for-symbol-at-point
       :desc "Org agenda"                   "a" #'org-agenda
       (:when (featurep! :tools biblio)
        :desc "Bibliographic entries"        "b"
        (cond ((featurep! :completion vertico)  #'citar-open-entry)
              ((featurep! :completion ivy)      #'ivy-bibtex)
              ((featurep! :completion helm)     #'helm-bibtex)))

       :desc "Toggle last org-clock"        "c" #'+org/toggle-last-clock
       :desc "Cancel current org-clock"     "C" #'org-clock-cancel
       :desc "Open deft"                    "d" #'deft
       (:when (featurep! :lang org +noter)
        :desc "Org noter"                  "e" #'org-noter)

       :desc "Find file in notes"           "f" #'+default/find-in-notes
       :desc "Browse notes"                 "F" #'+default/browse-notes
       :desc "Org store link"               "l" #'org-store-link
       :desc "Tags search"                  "m" #'org-tags-view
       :desc "Org capture"                  "n" #'org-capture
       :desc "Goto capture"                 "N" #'org-capture-goto-target
       :desc "Active org-clock"             "o" #'org-clock-goto
       :desc "Todo list"                    "t" #'org-todo-list
       :desc "Search notes"                 "s" #'+default/org-notes-search
       :desc "Search org agenda headlines"  "S" #'+default/org-notes-headlines
       :desc "View search"                  "v" #'org-search-view
       :desc "Org export to clipboard"        "y" #'+org/export-to-clipboard
       :desc "Org export to clipboard as RTF" "Y" #'+org/export-to-clipboard-as-rich-text

       (:when (featurep! :lang org +roam)
        (:prefix ("r" . "roam")
         :desc "Switch to buffer"              "b" #'org-roam-switch-to-buffer
         :desc "Org Roam Capture"              "c" #'org-roam-capture
         :desc "Find file"                     "f" #'org-roam-find-file
         :desc "Show graph"                    "g" #'org-roam-graph
         :desc "Insert"                        "i" #'org-roam-insert
         :desc "Insert (skipping org-capture)" "I" #'org-roam-insert-immediate
         :desc "Org Roam"                      "r" #'org-roam
         (:prefix ("d" . "by date")
          :desc "Arbitrary date" "d" #'org-roam-dailies-find-date
          :desc "Today"          "t" #'org-roam-dailies-find-today
          :desc "Tomorrow"       "m" #'org-roam-dailies-find-tomorrow
          :desc "Yesterday"      "y" #'org-roam-dailies-find-yesterday)))

       (:when (featurep! :lang org +roam2)
        (:prefix ("r" . "roam")
         :desc "Open random node"           "a" #'org-roam-node-random
         :desc "Find node"                  "f" #'org-roam-node-find
         :desc "Find ref"                   "F" #'org-roam-ref-find
         :desc "Show graph"                 "g" #'org-roam-graph
         :desc "Insert node"                "i" #'org-roam-node-insert
         :desc "Capture to node"            "n" #'org-roam-capture
         :desc "Toggle roam buffer"         "r" #'org-roam-buffer-toggle
         :desc "Launch roam buffer"         "R" #'org-roam-buffer-display-dedicated
         :desc "Sync database"              "s" #'org-roam-db-sync
         (:prefix ("d" . "by date")
          :desc "Goto previous note"        "b" #'org-roam-dailies-goto-previous-note
          :desc "Goto date"                 "d" #'org-roam-dailies-goto-date
          :desc "Capture date"              "D" #'org-roam-dailies-capture-date
          :desc "Goto next note"            "f" #'org-roam-dailies-goto-next-note
          :desc "Goto tomorrow"             "m" #'org-roam-dailies-goto-tomorrow
          :desc "Capture tomorrow"          "M" #'org-roam-dailies-capture-tomorrow
          :desc "Capture today"             "n" #'org-roam-dailies-capture-today
          :desc "Goto today"                "t" #'org-roam-dailies-goto-today
          :desc "Capture today"             "T" #'org-roam-dailies-capture-today
          :desc "Goto yesterday"            "y" #'org-roam-dailies-goto-yesterday
          :desc "Capture yesterday"         "Y" #'org-roam-dailies-capture-yesterday
          :desc "Find directory"            "-" #'org-roam-dailies-find-directory)))

       (:when (featurep! :lang org +journal)
        (:prefix ("j" . "journal")
         :desc "New Entry"           "j" #'org-journal-new-entry
         :desc "New Scheduled Entry" "J" #'org-journal-new-scheduled-entry
         :desc "Search Forever"      "s" #'org-journal-search-forever)))

      ;;; <leader> o --- open
      (:prefix-map ("o" . "open")
       :desc "Imenu sidebar"    "i"  #'imenu-list-smart-toggle
       :desc "Org agenda"       "A"  #'org-agenda
       (:prefix ("a" . "org agenda")
        :desc "Agenda"         "a"  #'org-agenda
        :desc "Todo list"      "t"  #'org-todo-list
        :desc "Tags search"    "m"  #'org-tags-view
        :desc "View search"    "v"  #'org-search-view)
       :desc "Default browser"    "b"  #'browse-url-of-file
       :desc "Start debugger"     "d"  #'+debugger/start
       :desc "New frame"          "f"  #'make-frame
       :desc "Select frame"       "F"  #'select-frame-by-name
       :desc "REPL"               "r"  #'+eval/open-repl-other-window
       :desc "REPL (same window)" "R"  #'+eval/open-repl-same-window
       :desc "Dired"              "-"  #'dired-jump
       (:when (featurep! :editor neotree)
        :desc "Project sidebar"              "p" #'+neotree/open
        :desc "Find file in project sidebar" "P" #'+neotree/find-this-file)
       (:when (featurep! :editor treemacs)
        :desc "Open project sidebar" "P" #'treemacs-select-window
        :desc "Toggle project sidebar" "p" #'+treemacs/toggle)
       (:when (featurep! :term vterm)
        :desc "Toggle vterm popup"    "t" #'+vterm/toggle
        :desc "Open vterm here"       "T" #'+vterm/here)
       (:when (featurep! :term eshell)
        :desc "Toggle eshell popup"   "e" #'+eshell/toggle
        :desc "Open eshell here"      "E" #'+eshell/here))

      ;;; <leader> p --- project
      (:prefix-map ("p" . "project")
       :desc "Search project"               "n" #'+default/evil-search-to-project
       :desc "Run shell in project"         "'" #'+vterm/here
       :desc "Browse project"               "." #'+default/browse-project
       :desc "Browse other project"         ">" #'doom/browse-in-other-project
       :desc "Run cmd in project root"      "!" #'projectile-run-shell-command-in-root
       :desc "Add new project"              "a" #'projectile-add-known-project
       :desc "Switch project buffer"        "b" #'counsel-projectile-switch-to-buffer
       :desc "Compile in project"           "c" #'projectile-compile-project
       :desc "Repeat last command"          "C" #'projectile-repeat-last-command
       :desc "Remove known project"         "d" #'projectile-remove-known-project
       :desc "Discover projects in folder"  "D" #'+default/discover-projects
       :desc "Edit project .dir-locals"     "e" #'projectile-edit-dir-locals
       :desc "Find file in project"         "f" #'projectile-find-file
       :desc "Find file in other project"   "F" #'doom/find-file-in-other-project
       :desc "Configure project"            "g" #'projectile-configure-project
       :desc "Invalidate project cache"     "i" #'projectile-invalidate-cache
       :desc "Kill project buffers"         "k" #'projectile-kill-buffers
       :desc "Find other file"              "o" #'projectile-find-other-file
       :desc "Switch project"               "p" #'projectile-switch-project
       :desc "Find recent project files"    "r" #'projectile-recentf
       :desc "Run project"                  "R" #'projectile-run-project
       :desc "Save project files"           "s" #'projectile-save-project-buffers
       :desc "List project todos"           "t" #'magit-todos-list
       :desc "Test project"                 "T" #'projectile-test-project
       :desc "Pop up scratch buffer"        "x" #'doom/open-project-scratch-buffer
       :desc "Switch to scratch buffer"     "X" #'doom/switch-to-project-scratch-buffer
       (:when (and (featurep! :tools taskrunner)
                   (featurep! :completion ivy))
        :desc "List project tasks"          "z" #'+taskrunner/project-tasks))

      ;;; <leader> q --- quit/session
      (:prefix-map ("q" . "quit/session")
       :desc "Restart emacs server"         "d" #'+default/restart-server
       :desc "Delete frame"                 "f" #'delete-frame
       :desc "Clear current frame"          "F" #'doom/kill-all-buffers
       :desc "Kill Emacs (and daemon)"      "K" #'save-buffers-kill-emacs
       :desc "Quit Emacs"                   "q" #'save-buffers-kill-terminal
       :desc "Quit Emacs without saving"    "Q" #'evil-quit-all-with-error-code
       :desc "Quick save current session"   "s" #'doom/quicksave-session
       :desc "Restore last session"         "l" #'doom/quickload-session
       :desc "Save session to file"         "S" #'doom/save-session
       :desc "Restore session from file"    "L" #'doom/load-session
       :desc "Restart & restore Emacs"      "r" #'doom/restart-and-restore
       :desc "Restart Emacs"                "R" #'doom/restart)

      ;;; <leader> s --- search
      (:prefix-map ("s" . "search")
       :desc "Search buffer"                "b"
       (cond ((featurep! :completion vertico)   #'+default/search-buffer)
             ((featurep! :completion ivy)       #'swiper)
             ((featurep! :completion helm)      #'swiper))
       :desc "Search all open buffers"      "B"
       (cond ((featurep! :completion vertico)   (cmd!! #'consult-line-multi 'all-buffers))
             ((featurep! :completion ivy)       #'swiper-all)
             ((featurep! :completion helm)      #'swiper-all))
       :desc "Search current directory"     "d" #'+default/search-cwd
       :desc "Search other directory"       "D" #'+default/search-other-cwd
       :desc "Search .emacs.d"              "e" #'+default/search-emacsd
       :desc "Locate file"                  "f" #'locate
       :desc "Jump to symbol"               "i" #'imenu
       :desc "Jump to visible link"         "l" #'link-hint-open-link
       :desc "Jump to link"                 "L" #'ffap-menu
       :desc "Jump list"                    "j" #'evil-show-jumps
       :desc "Jump to bookmark"             "m" #'bookmark-jump
       :desc "Look up online"               "o" #'+lookup/online
       :desc "Look up online (w/ prompt)"   "O" #'+lookup/online-select
       :desc "Look up in local docsets"     "k" #'+lookup/in-docsets
       :desc "Look up in all docsets"       "K" #'+lookup/in-all-docsets
       :desc "Search project"               "p" #'+default/search-project
       :desc "Search other project"         "P" #'+default/search-other-project
       :desc "Jump to mark"                 "r" #'evil-show-marks
       :desc "Search buffer"                "s" #'+default/search-buffer
       :desc "Search buffer for thing at point" "S"
       (cond ((featurep! :completion vertico)   #'+vertico/search-symbol-at-point)
             ((featurep! :completion ivy)       #'swiper-isearch-thing-at-point)
             ((featurep! :completion helm)      #'swiper-isearch-thing-at-point))
       :desc "Dictionary"                   "t" #'+lookup/dictionary-definition
       :desc "Thesaurus"                    "T" #'+lookup/synonyms)

      ;;; <leader> t --- toggle
      (:prefix-map ("t" . "toggle")
       :desc "Big mode"                     "b" #'doom-big-font-mode
       :desc "Fill Column Indicator"        "c" #'global-display-fill-column-indicator-mode
       :desc "Flymake"                      "f" #'flymake-mode
       (:when (featurep! :checkers syntax)
        :desc "Flycheck"                   "f" #'flycheck-mode)
       :desc "Frame fullscreen"             "F" #'toggle-frame-fullscreen
       :desc "Evil goggles"                 "g" #'evil-goggles-mode
       (:when (featurep! :editor indent-guides)
        :desc "Indent guides"              "i" #'highlight-indent-guides-mode)
       :desc "Indent style"                 "I" #'doom/toggle-indent-style
       :desc "Line numbers"                 "l" #'doom/toggle-line-numbers
       (:when (featurep! :lang org +present)
        :desc "org-tree-slide mode"        "p" #'org-tree-slide-mode)
       :desc "Read-only mode"               "r" #'read-only-mode
       (:when (featurep! :lang org +pomodoro)
        :desc "Pomodoro timer"             "t" #'org-pomodoro)
       :desc "Soft line wrapping"           "w" #'visual-line-mode
       (:when (featurep! :editor word-wrap)
        :desc "Soft line wrapping"         "w" #'+word-wrap-mode))

      ;;; <leader> w --- window
      (:prefix-map ("w" . "window")
       :desc "Alternate window"           "TAB" #'+default/alternate-window
       :desc "Other window"               "w"   #'other-window
       (:when (featurep! :editor tabs)
        :desc "Tab hydra"                 "t"   #'tabs-fast-switch/body)
       :desc "Split window right"         "v"   #'split-window-right
       :desc "Split window right"         "|"   #'split-window-right
       :desc "Split window below"         "s"   #'split-window-below
       :desc "Split window below"         "-"   #'split-window-below
       :desc "Balance window"             "="   #'balance-windows
       :desc "Switch to left"             "h"   #'evil-window-left
       :desc "Switch to right"            "l"   #'evil-window-right
       :desc "Switch to up"               "k"   #'evil-window-up
       :desc "Switch to down"             "j"   #'evil-window-down
       :desc "Kill other window"          "O"   #'ace-delete-other-windows
       :desc "Kill other window"          "o"   #'delete-other-windows
       :desc "Kill window"                "D"   #'ace-delete-window
       :desc "Kill current window"        "d"   #'delete-window))

(after! which-key
  (let ((prefix-re (regexp-opt (list doom-leader-key doom-leader-alt-key))))
    (cl-pushnew `((,(format "\\`\\(?:C-w\\|%s w\\) m\\'" prefix-re))
                  nil . "maximize")
                which-key-replacement-alist)))
