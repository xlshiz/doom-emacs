;;; editor/tabs/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +tabs-buffer-list ()
  (let ((bufs (buffer-list)))
    (cl-remove-if #'+tabs-need-hide-p bufs)))

(defun +tabs-hide-buffer-p (buf)
  (let* ((name (buffer-name buf))
         (use-workspace (modulep! :editor workspaces))
         (blacklist '("*" " *" "  *"
                      "COMMIT_EDITMSG")))
    (or
     (if use-workspace
         (not (+workspace-contains-buffer-p buf (+workspace-current)))
       nil)
     (window-dedicated-p (selected-window))
     (cl-some (lambda (prefix) (string-prefix-p prefix name)) blacklist)
     (eq (aref name 0) ?\s))))

(defun +tabs-whitelist-buffer-p (buf)
  (let* ((name (buffer-name buf))
         (whitelist '("*vterm*" "*forge:"
                     "*scratch*" "*Messages*"
                     "*Org Agenda*")))
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
  (if (modulep! +sort)
      (sort-tab-select-prev-tab)
    (awesome-tab-backward-tab)))

;;;###autoload
(defun +tabs/next ()
  "Select next tab"
  (interactive)
  (if (modulep! +sort)
      (sort-tab-select-next-tab)
    (awesome-tab-forward-tab)))

;;;###autoload
(defun +tabs/ace-jump ()
  "Select next tab"
  (interactive)
  (if (modulep! +sort)
      (awesome-tab-ace-jump)
    (awesome-tab-ace-jump)))
