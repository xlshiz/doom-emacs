;;; tools/lookup/config.el -*- lexical-binding: t; -*-

;; "What am I looking at?" This module helps you answer this question.
;;
;;   + `+lookup/definition': a jump-to-definition that should 'just work'
;;   + `+lookup/implementations': find a symbol's implementations in the current
;;                                project
;;   + `+lookup/references': find a symbol's references in the current project
;;   + `+lookup/file': open the file referenced at point
;;   + `+lookup/in-docsets': look up in Dash docsets
;;
;; This module uses `xref', an experimental new library in Emacs. It may change
;; in the future. When xref can't be depended on it will fall back to
;; `dumb-jump' to find what you want.

(defvar +lookup-definition-functions
  '(+lookup-dictionary-definition-backend-fn
    +lookup-xref-definitions-backend-fn
    +lookup-dumb-jump-backend-fn
    +lookup-project-search-backend-fn
    +lookup-evil-goto-definition-backend-fn)
  "Functions for `+lookup/definition' to try, before resorting to `dumb-jump'.
Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-implementations-functions ()
  "Function for `+lookup/implementations' to try. Stops at the first function to
return non-nil or change the current window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-type-definition-functions ()
  "Functions for `+lookup/type-definition' to try. Stops at the first function to
return non-nil or change the current window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-references-functions
  '(+lookup-thesaurus-definition-backend-fn
    +lookup-xref-references-backend-fn
    +lookup-project-search-backend-fn)
  "Functions for `+lookup/references' to try, before resorting to `dumb-jump'.
Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-documentation-functions
  nil
  "Functions for `+lookup/documentation' to try, before resorting to
`dumb-jump'. Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-file-functions
  '(+lookup-bug-reference-backend-fn
    +lookup-ffap-backend-fn)
  "Function for `+lookup/file' to try, before restoring to `find-file-at-point'.
Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-dictionary-prefer-offline (featurep! +offline)
  "If non-nil, look up dictionaries online.

Setting this to nil will force it to use offline backends, which may be less
than perfect, but available without an internet connection.

Used by `+lookup/dictionary-definition' and `+lookup/synonyms'.

For `+lookup/dictionary-definition', this is ignored on Mac, where Emacs users
Dictionary.app behind the scenes to get definitions.")


;;
;;; dumb-jump

(use-package! dumb-jump
  :commands dumb-jump-result-follow
  :config
  (setq dumb-jump-default-project doom-emacs-dir
        dumb-jump-prefer-searcher 'rg
        dumb-jump-aggressive nil
        dumb-jump-selector
        (cond ('popup)))
  (add-hook 'dumb-jump-after-jump-hook #'better-jumper-set-jump))


;;
;;; xref

;; The lookup commands are superior, and will consult xref if there are no
;; better backends available.
(global-set-key [remap xref-find-definitions] #'+lookup/definition)
(global-set-key [remap xref-find-references]  #'+lookup/references)

(after! xref
  ;; We already have `projectile-find-tag' and `evil-jump-to-tag', no need for
  ;; xref to be one too.
  (remove-hook 'xref-backend-functions #'etags--xref-backend)
  ;; ...however, it breaks `projectile-find-tag', unless we put it back.
  (defadvice! +lookup--projectile-find-tag-a (fn)
    :around #'projectile-find-tag
    (let ((xref-backend-functions '(etags--xref-backend t)))
      (funcall fn)))

  ;; This integration is already built into evil
  (unless (featurep! :editor evil)
    ;; Use `better-jumper' instead of xref's marker stack
    (advice-add #'xref-push-marker-stack :around #'doom-set-jump-a))

  (use-package! consult-xref
    :when (featurep! :completion vertico)
    :defer t
    :init
    (setq xref-show-xrefs-function       #'consult-xref
          xref-show-definitions-function #'consult-xref)))


;;
;;; Dash docset integration

(use-package! dash-docs
  :when (featurep! +docsets)
  :defer t
  :init
  (add-hook '+lookup-documentation-functions #'+lookup-dash-docsets-backend-fn)
  :config
  (setq dash-docs-enable-debugging doom-debug-p
        dash-docs-docsets-path (concat doom-etc-dir "docsets/")
        dash-docs-min-length 2
        dash-docs-browser-func #'eww))


;;
;;; Dictionary integration

(use-package! define-word
  :when (featurep! +dictionary)
  :unless IS-MAC
  :defer t
  :config
  (setq define-word-displayfn-alist
        (cl-loop for (service . _) in define-word-services
                 collect (cons service #'+eval-display-results-in-popup))))


;;;###package synosaurus
(setq synosaurus-choose-method 'default)
