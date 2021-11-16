;;; editor/tabs/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +tabs/prev ()
  "Select prev tab"
  (interactive)
  (if (featurep! +sort)
      (sort-tab-select-prev-tab)
    (awesome-tab-backward-tab)))

;;;###autoload
(defun +tabs/next ()
  "Select next tab"
  (interactive)
  (if (featurep! +sort)
      (sort-tab-select-next-tab)
    (awesome-tab-forward-tab)))

;;;###autoload
(defun +tabs/ace-jump ()
  "Select next tab"
  (interactive)
  (if (featurep! +sort)
      (awesome-tab-ace-jump)
    (awesome-tab-ace-jump)))
