;;; app/taskrunner/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +taskrunner/project-tasks ()
  "Show task in project."
  (interactive)
  (cond ((featurep! :completion ivy) (ivy-taskrunner))
        ((featurep! :completion helm) (helm-taskrunner))))
