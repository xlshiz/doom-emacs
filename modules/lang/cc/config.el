;;; lang/cc/config.el --- c, c++, and obj-c -*- lexical-binding: t; -*-

(defvar +cc-default-include-paths
  (list "include"
        "includes")
  "A list of default relative paths which will be searched for up from the
current file, to be passed to irony as extra header search paths. Paths can be
absolute. This is ignored if your project has a compilation database.

This is ignored by ccls.")

(defvar +cc-default-header-file-mode 'c-mode
  "Fallback major mode for .h files if all other heuristics fail (in
`+cc-c-c++-objc-mode').")

(defvar +cc-default-compiler-options
  `((c-mode . nil)
    (c++-mode
     . ,(list "-std=c++1z" ; use C++17 draft by default
              (when IS-MAC
                ;; NOTE beware: you'll get abi-inconsistencies when passing
                ;; std-objects to libraries linked with libstdc++ (e.g. if you
                ;; use boost which wasn't compiled with libc++)
                "-stdlib=libc++")))
    (objc-mode . nil))
  "A list of default compiler options for the C family. These are ignored if a
compilation database is present in the project.

This is ignored by ccls.")


;;
;;; Packages

(use-package! cc-mode
  :mode ("\\.mm\\'" . objc-mode)
  ;; Use `c-mode'/`c++-mode'/`objc-mode' depending on heuristics
  :mode ("\\.h\\'" . +cc-c-c++-objc-mode)
  ;; Ensure find-file-at-point recognize system libraries in C modes. It must be
  ;; set up before the likes of irony/lsp are initialized. Also, we use
  ;; local-vars hooks to ensure these only run in their respective major modes,
  ;; and not their derived modes.
  :hook ((c-mode-local-vars c++-mode-local-vars objc-mode-local-vars) . +cc-init-ffap-integration-h)
  ;;; Improve fontification in C/C++ (also see `modern-cpp-font-lock')
  :hook (c-mode-common . rainbow-delimiters-mode)
  :hook ((c-mode c++-mode) . +cc-fontify-constants-h)
  :hook ((c-mode c++-mode) . +cc-disable-auto-complete)
  :init
  (add-to-list 'doom-detect-indentation-excluded-modes 'c-mode)
  (add-to-list 'doom-detect-indentation-excluded-modes 'c++-mode)
  :config
  (set-docsets! 'c-mode "C")
  (set-docsets! 'c++-mode "C++" "Boost")
  (set-electric! '(c-mode c++-mode objc-mode java-mode) :chars '(?\n ?\} ?\{))
  (set-formatter!
    'clang-format
    '("clang-format"
      "-assume-filename"
      (or (buffer-file-name)
          (cdr (assoc major-mode
                      '((c-mode        . ".c")
                        (c++-mode      . ".cpp")
                        (cuda-mode     . ".cu")
                        (protobuf-mode . ".proto"))))))
    :modes '(c-mode c++-mode protobuf-mode cuda-mode))
  (when (modulep! +tree-sitter)
    (add-hook! '(c-mode-local-vars-hook
                 c++-mode-local-vars-hook)
               :append #'tree-sitter!))

  ;; HACK Suppress 'Args out of range' error in when multiple modifications are
  ;;      performed at once in a `c++-mode' buffer, e.g. with `iedit' or
  ;;      multiple cursors.
  (undefadvice! +cc--suppress-silly-errors-a (fn &rest args)
    :around #'c-after-change-mark-abnormal-strings
    (ignore-errors (apply fn args)))

  ;; Custom style, based off of linux
  (setq c-basic-offset tab-width
        c-backspace-function #'delete-backward-char)

  (c-add-style
   "doom" '("linux"
            (c-comment-only-line-offset . 0)
            (tab-width . 8)
            (c-basic-offset . 8)
            (indent-tabs-mode . t)
            (c-hanging-braces-alist (brace-list-open)
                                    (brace-entry-open)
                                    (substatement-open after)
                                    (block-close . c-snug-do-while)
                                    (arglist-cont-nonempty))
            (c-cleanup-list brace-else-brace)
            (c-offsets-alist
             (knr-argdecl-intro . 0)
             (substatement-open . 0)
             (substatement-label . 0)
             (statement-cont . +)
             ;; align args with open brace OR don't indent at all (if open
             ;; brace is at eolp and close brace is after arg with no trailing
             ;; comma)
             (brace-list-intro . 0)
             (brace-list-close . -)
             (arglist-intro . +)
             (arglist-close +cc-lineup-arglist-close 0)
             ;; don't over-indent lambda blocks
             (inline-open . 0)
             (inlambda . 0)
             ;; indent access keywords +1 level, and properties beneath them
             ;; another level
             (access-label . -)
             (inclass +cc-c++-lineup-inclass +)
             (label . 0))))

  (when (listp c-default-style)
    (setf (alist-get 'other c-default-style) "doom"))

  (after! ffap
    (add-to-list 'ffap-alist '(c-mode . ffap-c-mode)))
  (map! :map c-mode-map :n [remap pop-tag-mark]    #'citre-jump-back)
  (set-lookup-handlers! 'c-mode
    :definition #'citre-jump))

(use-package! modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))


;;
;; Major modes

(after! cmake-mode
  (set-docsets! 'cmake-mode "CMake")
  (set-popup-rule! "^\\*CMake Help\\*" :size 0.4 :ttl t)
  (set-lookup-handlers! 'cmake-mode
    :documentation '+cc-cmake-lookup-documentation-fn))


(use-package! company-cmake  ; for `cmake-mode'
  :when (modulep! :completion company)
  :after cmake-mode
  :config (set-company-backend! 'cmake-mode 'company-cmake))


(use-package! demangle-mode
  :hook llvm-mode)


(use-package! company-glsl  ; for `glsl-mode'
  :when (modulep! :completion company)
  :after glsl-mode
  :config (set-company-backend! 'glsl-mode 'company-glsl))


;;
;; LSP

(when (modulep! +lsp)
  ; (add-hook! '(c-mode-local-vars-hook
               ; c++-mode-local-vars-hook
               ; objc-mode-local-vars-hook
               ; cmake-mode-local-vars-hook)
             ; :append #'lsp!)

  (setq lsp-clients-clangd-args '("-j=3"
                                  "--background-index"
                                  "--clang-tidy"
                                  "--completion-style=detailed"
                                  "--header-insertion=never"
                                  "--header-insertion-decorators=0"))
  (unless (modulep! +ccls)
    (after! lsp-clangd (set-lsp-priority! 'clangd 1))))

(when (modulep! :completion vertico)
  (after! consult-imenu
    (add-to-list 'consult-imenu-config '(c-mode :toplevel "function"
                                                :types ((?f "function" font-lock-function-name-face)
                                                        (?m "macro"    font-lock-function-name-face)
                                                        (?p "prototype"  font-lock-constant-face)
                                                        (?v "variable" font-lock-variable-name-face)
                                                        (?t "typedef"      font-lock-type-face))))))
