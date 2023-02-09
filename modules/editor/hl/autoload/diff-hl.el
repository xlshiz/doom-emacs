;;; editor/hl/autoload/diff-hl.el -*- lexical-binding: t; -*-
;;;###if (modulep! +diff-hl)

;;;###autoload
(defalias '+vc-gutter/stage-hunk #'diff-hl-stage-current-hunk)
;;;###autoload
(defalias '+vc-gutter/revert-hunk #'diff-hl-revert-hunk)
;;;###autoload
(defalias '+vc-gutter/next-hunk #'diff-hl-next-hunk)
;;;###autoload
(defalias '+vc-gutter/previous-hunk #'diff-hl-previous-hunk)
;;;###autoload
(defalias '+vc-gutter/mark-hunk #'diff-hl-mark-hunk)

;;;###autoload (autoload '+vc/gutter-hydra/body "editor/hl/autoload/diff-hl" nil t)
(defhydra +vc/gutter-hydra
  (:body-pre (diff-hl-mode 1) :hint nil)
  "
       [git gutter]
   Movement  Actions
  ╭───────────────────────────╯
     ^_g_^       [_s_] stage
     ^_k_ ↑^     [_r_] revert
     ^_j_ ↓^     [_m_] mark
     ^_G_^"
  ("j" (progn (+vc-gutter/next-hunk) (recenter)))
  ("k" (progn (+vc-gutter/previous-hunk) (recenter)))
  ("g" (progn (goto-char (point-min)) (+vc-gutter/next-hunk)))
  ("G" (progn (goto-char (point-min)) (+vc-gutter/previous-hunk)))
  ("s" +vc-gutter/stage-hunk)
  ("r" +vc-gutter/revert-hunk)
  ("m" +vc-gutter/mark-hunk))
