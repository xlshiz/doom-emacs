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
