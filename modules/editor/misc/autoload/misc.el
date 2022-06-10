;;; editor/misc/autoload.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+vc/gutter-hydra/body "editor/misc/autoload/misc" nil t)
(defhydra +vc/gutter-hydra
  (:body-pre (git-gutter-mode 1) :hint nil)
  "
                                               [git gutter]
   Movement  Hunk Actions     Misc.         +%-4s(car (git-gutter:statistic))/ -%-4s(cdr (git-gutter:statistic))
  ╭──────────────────────────────────┴────────────────╯
     ^_g_^       [_s_] stage        [_R_] set start Rev
     ^_k_^       [_r_] revert
     ^↑ ^      [_m_] mark
     ^↓ ^      [_p_] popup             ╭─────────────────────
     ^_j_^                             │[_q_] quit
     ^_G_^                             │[_Q_] Quit and disable"
  ("j" (progn (git-gutter:next-hunk 1) (recenter)))
  ("k" (progn (git-gutter:previous-hunk 1) (recenter)))
  ("g" (progn (goto-char (point-min)) (git-gutter:next-hunk 1)))
  ("G" (progn (goto-char (point-min)) (git-gutter:previous-hunk 1)))
  ("s" git-gutter:stage-hunk)
  ("r" git-gutter:revert-hunk)
  ("m" git-gutter:mark-hunk)
  ("p" git-gutter:popup-hunk)
  ("R" git-gutter:set-start-revision)
  ("q"
   (when (get-buffer git-gutter:popup-buffer)
     (kill-buffer (get-buffer git-gutter:popup-buffer)))
   :color blue)
  ("Q"
   (progn (git-gutter-mode -1)
          (when (get-buffer git-gutter:popup-buffer)
            (kill-buffer (get-buffer git-gutter:popup-buffer))))
   :color blue))

;;;###autodef
(defun set-electric! (modes &rest plist)
  "Declare that WORDS (list of strings) or CHARS (lists of chars) should trigger
electric indentation.

Enables `electric-indent-local-mode' in MODES.

\(fn MODES &key WORDS CHARS)"
  (declare (indent defun))
  (dolist (mode (doom-enlist modes))
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
