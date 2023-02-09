;;; editor/hl/autoload/git-gutter.el -*- lexical-binding: t; -*-
;;;###if (not (modulep! +diff-hl))

;;;###autoload
(defalias '+vc-gutter/stage-hunk #'git-gutter:stage-hunk)
;;;###autoload
(defalias '+vc-gutter/revert-hunk #'git-gutter:revert-hunk)
;;;###autoload
(defalias '+vc-gutter/next-hunk #'git-gutter:next-hunk)
;;;###autoload
(defalias '+vc-gutter/previous-hunk #'git-gutter:previous-hunk)
;;;###autoload
(defalias '+vc-gutter/mark-hunk #'git-gutter:mark-hunk)

;;;###autoload (autoload '+vc/gutter-hydra/body "editor/hl/autoload/git-gutter" nil t)
(defhydra +vc/gutter-hydra
  (:body-pre (git-gutter-mode 1) :hint nil)
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
