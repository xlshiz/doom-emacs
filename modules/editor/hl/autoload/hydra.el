;;; editor/hl/autoload/hydra.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+vc/gutter-hydra/body "editor/hl/autoload/hydra" nil t)
(defhydra +vc/gutter-hydra
  (:hint nil)
  "
       [git gutter]
   Movement  Actions
  ╭───────────────────────────╯
     ^_g_^       [_s_] stage
     ^_k_ ↑^     [_r_] revert
     ^_j_ ↓^     [_m_] mark
     ^_G_^"
  ("j" (progn (+vc-gutter/next-hunk 1) (recenter)))
  ("k" (progn (+vc-gutter/previous-hunk 1) (recenter)))
  ("g" (progn (goto-char (point-min)) (+vc-gutter/next-hunk 1)))
  ("G" (progn (goto-char (point-min)) (+vc-gutter/previous-hunk 1)))
  ("s" +vc-gutter/stage-hunk)
  ("r" +vc-gutter/revert-hunk)
  ("m" +vc-gutter/mark-hunk))

;;;###autodef
(defun set-electric! (modes &rest plist)
  "Declare that WORDS (list of strings) or CHARS (lists of chars) should trigger
electric indentation.

Enables `electric-indent-local-mode' in MODES.

\(fn MODES &key WORDS CHARS)"
  (declare (indent defun))
  (dolist (mode (ensure-list modes))
    (let ((hook (intern (format "%s-hook" mode)))
          (fn   (intern (format "+electric--init-%s-h" mode))))
      (cond ((null (car-safe plist))
             (remove-hook hook fn)
             (unintern fn nil))
            ((fset
              fn (lambda ()
                   (when (eq major-mode mode)
                     (setq-local electric-indent-inhibit nil)
                     (cl-destructuring-bind (&key chars words) plist
                       (electric-indent-local-mode +1)
                       (if chars (setq-local electric-indent-chars chars))
                       (if words (setq +electric-indent-words words))))))
             (add-hook hook fn))))))
