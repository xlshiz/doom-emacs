;;; editor/search/autoload/snail-vertico.el -*- lexical-binding: t; -*-
;;;###if (featurep! :completion vertico)

;;;###autoload
(defun snail--project-files (&rest _)
  (require 'projectile)
  (let ((ht (consult--buffer-file-hash)))
    (unless (file-equal-p default-directory "~")
      (seq-remove (lambda (x)
                    (gethash (projectile-expand-root x) ht))
                  (mapcar #'substring-no-properties (if (doom-project-root)
                                                        (projectile-current-project-files)))))))

;;;###autoload
(defvar snail--source-buffer
  `(:name     "Buffer"
    :narrow   ?b
    :category buffer
    :face     consult-buffer
    :history  buffer-name-history
    :state    ,#'consult--buffer-state
    :default  t
    :items
    ,(lambda () (consult--buffer-query :sort 'visibility
                                       :exclude '(" .*" "\\*sort-tab\\*")
                                       :as #'buffer-name)))
  "Buffer candidate source for `snail'.")

;;;###autoload
(defvar snail--source-recent-file
  `(:name "Recent Files"
    :narrow ?r
    :category file
    :face consult-file
    :history file-name-history
    :state ,#'consult--file-state
    :action nil
    :require-match nil
    :items
    ,(lambda ()
       (let ((ht (consult--buffer-file-hash)))
         (recentf-mode)
         (mapcar #'abbreviate-file-name
                 (seq-remove (lambda (x)
                               (gethash x ht))
                             recentf-list)))))
  "Recent file candidate source for `snail'.")

;;;###autoload
(defvar snail--source-project-file
  `(:name     "Project File"
    :narrow   (?p . "Project")
    :category file
    :hidden   nil
    :action nil
    :require-match nil
    :items snail--project-files)
  "Project buffer candidate source for `snail'.")

;;;###autoload
(defvar snail-sources
  '(consult--source-hidden-buffer
    snail--source-buffer
    snail--source-project-file
    snail--source-recent-file))

;;;###autoload
(defun snail ()
  "Mix the `buffer' `projectile' and `recentf'"
  (interactive)
  (require 'consult)
  (when-let (sel (consult--multi snail-sources
                                 :require-match nil
                                 :prompt "Snail: "
                                 :history nil
                                 :sort nil))
    (if (cdr sel)
        (if (string= (plist-get (cdr sel) :name) "Project File")
            (find-file (projectile-expand-root (car sel))))
      (consult--buffer-action (car sel)))))
