;;; tools/pdf/config.el -*- lexical-binding: t; -*-

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
  (defadvice! +pdf--install-epdfinfo-a (orig-fn &rest args)
    "Install epdfinfo after the first PDF file, if needed."
    :around #'pdf-view-mode
    (if (file-executable-p pdf-info-epdfinfo-program)
        (apply orig-fn args)
      ;; If we remain in pdf-view-mode, it'll spit out cryptic errors. This
      ;; graceful failure is better UX.
      (fundamental-mode)
      (message "Viewing PDFs in Emacs requires epdfinfo. Use `M-x pdf-tools-install' to build it")))

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
  (defadvice! +pdf-suppress-large-file-prompts-a (orig-fn size op-type filename &optional offer-raw)
    :around #'abort-if-file-too-large
    (unless (string-match-p "\\.pdf\\'" filename)
      (funcall orig-fn size op-type filename offer-raw))))


(use-package! saveplace-pdf-view
  :after pdf-view)
