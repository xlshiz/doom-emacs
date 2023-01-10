;;; editor/pdf/config.el -*- lexical-binding: t; -*-

(use-package! pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :init
  (after! pdf-annot
    (defun +pdf-cleanup-windows-h ()
      "Kill left-over annotation buffers when the document is killed."
      (when (buffer-live-p pdf-annot-list-document-buffer)
        (pdf-info-close pdf-annot-list-document-buffer))
      (when (buffer-live-p pdf-annot-list-buffer)
        (kill-buffer pdf-annot-list-buffer))
      (let ((contents-buffer (get-buffer "*Contents*")))
        (when (and contents-buffer (buffer-live-p contents-buffer))
          (kill-buffer contents-buffer))))
    (add-hook! 'pdf-view-mode-hook
      (add-hook 'kill-buffer-hook #'+pdf-cleanup-windows-h nil t)))

  :config
  (defadvice! +pdf--install-epdfinfo-a (fn &rest args)
    "Install epdfinfo after the first PDF file, if needed."
    :around #'pdf-view-mode
    (if (and (require 'pdf-info nil t)
             (or (pdf-info-running-p)
                 (ignore-errors (pdf-info-check-epdfinfo) t)))
        (apply fn args)
      ;; If we remain in pdf-view-mode, it'll spit out cryptic errors. This
      ;; graceful failure is better UX.
      (fundamental-mode)
      (message "Viewing PDFs in Emacs requires epdfinfo. Use `M-x pdf-tools-install' to build it")))

  ;; Despite its namesake, this does not call `pdf-tools-install', it only sets
  ;; up hooks, auto-mode-alist/magic-mode-alist entries, global modes, and
  ;; refreshes pdf-view-mode buffers, if any.
  ;;
  ;; I avoid calling `pdf-tools-install' directly because `pdf-tools' is easy to
  ;; prematurely load in the background (e.g. when exporting an org file or by
  ;; packages like org-pdftools). And I don't want pdf-tools to suddenly block
  ;; Emacs and spew out compiler output for a few minutes in those cases. It's
  ;; abysmal UX. The `pdf-view-mode' advice above works around this with a less
  ;; cryptic failure message, at least.
  (pdf-tools-install-noverify)

  ;; For consistency with other special modes
  (map! :map pdf-view-mode-map :gn "q" #'kill-current-buffer)

  (setq-default pdf-view-display-size 'fit-page)
  ;; Enable hiDPI support, but at the cost of memory! See politza/pdf-tools#51
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)

  ;; Handle PDF-tools related popups better
  (set-popup-rules!
    '(("^\\*Outline*" :side right :size 40 :select nil)
      ("^\\*Edit Annotation " :quit nil)
      ("\\(?:^\\*Contents\\|'s annots\\*$\\)" :ignore t)))

  ;; The mode-line does serve any useful purpose is annotation windows
  (add-hook 'pdf-annot-list-mode-hook #'hide-mode-line-mode)

  ;; HACK Fix #1107: flickering pdfs when evil-mode is enabled
  (setq-hook! 'pdf-view-mode-hook evil-normal-state-cursor (list nil))

  ;; HACK Refresh FG/BG for pdfs when `pdf-view-midnight-colors' is changed by a
  ;;      theme or with `setq!'.
  ;; TODO PR this upstream?
  (defun +pdf-reload-midnight-minor-mode-h ()
    (when pdf-view-midnight-minor-mode
      (pdf-info-setoptions
       :render/foreground (car pdf-view-midnight-colors)
       :render/background (cdr pdf-view-midnight-colors)
       :render/usecolors t)
      (pdf-cache-clear-images)
      (pdf-view-redisplay t)))
  (put 'pdf-view-midnight-colors 'custom-set
       (lambda (sym value)
         (set-default sym value)
         (dolist (buffer (doom-buffers-in-mode 'pdf-view-mode))
           (with-current-buffer buffer
             (if (get-buffer-window buffer)
                 (+pdf-reload-midnight-minor-mode-h)
               ;; Defer refresh for buffers that aren't visible, to avoid
               ;; blocking Emacs for too long while changing themes.
               (add-hook 'doom-switch-buffer-hook #'+pdf-reload-midnight-minor-mode-h
                         nil 'local))))))

  ;; Silence "File *.pdf is large (X MiB), really open?" prompts for pdfs
  (defadvice! +pdf-suppress-large-file-prompts-a (fn size op-type filename &optional offer-raw)
    :around #'abort-if-file-too-large
    (unless (string-match-p "\\.pdf\\'" filename)
      (funcall fn size op-type filename offer-raw))))

(after! pdf-tools
  (defun my/isearch-failed? ()
    (or (not isearch-success) isearch-error))

  (defvar-local my/pdf-isearch-highlight-matches nil)
  (defun my/pdf-isearch-cleanup-highlight ()
    (setq my/pdf-isearch-highlight-matches nil)
    (pdf-view-redisplay))

  ;; 模仿occur-hack-p，注入变量以控制高亮
  (defun my/pdf-isearch-hl-matches-controllable-highlight (orig-fun current matches &optional occur-hack-p)
    (funcall orig-fun current matches (or my/pdf-isearch-highlight-matches occur-hack-p)))
  (advice-add #'pdf-isearch-hl-matches
    :around #'my/pdf-isearch-hl-matches-controllable-highlight)

  (defvar-local my/pdf-isearch-forward? t)

  (defun my/pdf-isearch-forward (&optional regexp-p no-recursive-edit)
    (interactive "P\np")
    (setq my/pdf-isearch-forward? t)
    (isearch-forward regexp-p no-recursive-edit))

  (defun my/pdf-isearch-backward (&optional regexp-p no-recursive-edit)
    (interactive "P\np")
    (setq my/pdf-isearch-forward? nil)
    (isearch-backward regexp-p no-recursive-edit))

  ;; 下面参数arg，原接口是raw prefix，不是数字prefix，
  ;; 这里保持一致，我不知道怎么反转raw prefix（不用hack的方式，
  ;; 比如操作raw prefix列表），不然my/pdf-isearch-repeat-backward
  ;; 可以简单反转下prefix调用my/pdf-isearch-repeat-backward，而不用写两遍。
  (defun my/pdf-isearch-repeat-forward (&optional arg)
    (interactive "P")
    (setq my/pdf-isearch-highlight-matches t)
    (if my/pdf-isearch-forward?
      (isearch-repeat-forward arg)
      (isearch-repeat-backward arg)))

  (defun my/pdf-isearch-repeat-backward (&optional arg)
    (interactive "P")
    (setq my/pdf-isearch-highlight-matches t)
    (if my/pdf-isearch-forward?
      (isearch-repeat-backward arg)
      (isearch-repeat-forward arg)))

  ;; 搜索超过文档结尾时，直接wrap到文档头开始搜索，不要暂停，
  ;; 如果不这样设置，则搜索超过文档结尾时，暂停的时候，结尾的高亮会不见
  ;; 估计不难修复，但是这样搞更简单。
  (add-hook 'pdf-view-mode-hook
    (lambda () (setq-local isearch-wrap-pause 'no)))

  (defun my/pdf-view-force-normal-state ()
    (interactive)
    (evil-force-normal-state)
    (my/pdf-isearch-cleanup-highlight))

  (advice-add #'pdf-isearch-mode-initialize
    :before
    (lambda (&rest args)
      "在正常使用C-s等进行搜索的时候，重置 `my/pdf-isearch-highlight-matches'。"
      (setq my/pdf-isearch-highlight-matches nil)))

  (defun my/pdf-isearch-mode-cleanup ()
    "按/键搜索，如果搜索成功，则按回车后不要清除高亮，如果搜索失败，则清除高亮。"
    (pdf-isearch-active-mode -1)
    (when (my/isearch-failed?) (pdf-view-redisplay)))
  (advice-add #'pdf-isearch-mode-cleanup
    :override #'my/pdf-isearch-mode-cleanup)

  (defmacro my/modify-evil-collection-key-bindings (mode &rest body)
    (declare (indent defun))
    (let ((feature-name-var (gensym "feature-name"))
           (mode-var (gensym "mode"))
           (mode-str-var (gensym "mode-str"))
           (hook-fun-name-var (gensym "hook-fun-name-var")))
      `(let* ((,mode-var ,mode)
               (,mode-str-var (symbol-name ,mode-var))
               (,feature-name-var (intern (concat "evil-collection-" ,mode-str-var))))
         (if (featurep ,feature-name-var)
           ;; 在当前evil-collection的实现中，如果feature名对应的文件
           ;; 已经加载，则对应的按键setup函数也已经调用，故在这种情况下，
           ;; 我们可以直接执行body。
           (progn ,@body)
           ;; 否则，添加hook到`evil-collection-setup-hook'，
           ;; 判断mode名匹配后执行body。
           (let ((,hook-fun-name-var
                   ;; 这里使用gensym生成hook函数名，由于生成的符号是未intern的，
                   ;; 这会导致evil-collection无法调用我们的hook函数，于是我们需要
                   ;; 提前intern下，同时由于gensym会在符号名后面附加一个全局计数器，
                   ;; 加上我们的函数有前缀 my/ ，因此一般不会产生名称冲突。
                   (intern
                     (symbol-name
                       (gensym (concat "my/modify-evil-collection-key-bindings-"
                                 ,mode-str-var))))))
             ;; 使用defun底层使用的defalias，不然defun不会eval函数名参数。
             (defalias ,hook-fun-name-var
               (lambda (mode keymaps)
                 (when (eq mode ,mode-var)
                   ,@body))
               (concat "修改evil-collection设置的" ,mode-str-var "的按键绑定。"))
             (add-hook 'evil-collection-setup-hook ,hook-fun-name-var))))))

  (my/modify-evil-collection-key-bindings 'pdf
    (evil-define-key 'normal pdf-view-mode-map
      ;; 按/或者?开始向前或者向后搜索并记住搜索方向，
      ;; 按n/N继续向前/向后搜索，
      ;; 按ESC则返回normal state的同时清除搜索高亮
      (kbd "/") #'my/pdf-isearch-forward
      (kbd "?") #'my/pdf-isearch-backward
      (kbd "n") #'my/pdf-isearch-repeat-forward
      (kbd "N") #'my/pdf-isearch-repeat-backward
      (kbd "<escape>") #'my/pdf-view-force-normal-state)))


(use-package! saveplace-pdf-view
  :after pdf-view)
