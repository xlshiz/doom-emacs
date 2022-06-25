;;; editor/tabs/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +tabs-buffer-list ()
  (let* ((bufs (buffer-list))
         (use-workspace (featurep! :editor workspaces))
         (active-workspace))
    (when use-workspace
        (setq active-workspace (+workspace-current))
        (setq bufs (cl-remove-if (lambda (b)
                                   (not (+workspace-contains-buffer-p b active-workspace ))) bufs)))
    (cl-remove-if #'+tabs-need-hide-p bufs)))

(defun +tabs-hide-buffer-p (buf)
  (let ((name (buffer-name buf))
        (blacklist '("*" " *" "  *"
                     "COMMIT_EDITMSG")))
    (or
     (window-dedicated-p (selected-window))
     (cl-some (lambda (prefix) (string-prefix-p prefix name)) blacklist)
     (eq (aref name 0) ?\s))))

(defun +tabs-whitelist-buffer-p (buf)
  (let* ((name (buffer-name buf))
         (whitelist '("*vterm*"
                     "*scratch*" "*Messages*")))
    (or
     (and (eq (current-buffer) buf) (not (string-prefix-p "*sort-tab*" name)))
     (cl-some (lambda (prefix) (string-prefix-p prefix name)) whitelist)
     )))

;;;###autoload
(defun +tabs-need-hide-p (buf)
  (if (+tabs-whitelist-buffer-p buf)
      nil
    (+tabs-hide-buffer-p buf)))

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
