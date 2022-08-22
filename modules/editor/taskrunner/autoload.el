;;; app/taskrunner/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +taskrunner/project-tasks ()
  "Show task in project."
  (interactive)
  (cond ((modulep! :completion ivy) (ivy-taskrunner))
        ((modulep! :completion helm) (helm-taskrunner))))
