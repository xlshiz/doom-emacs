;;; editor/search/autoload/snail-vertico.el -*- lexical-binding: t; -*-
;;;###if (featurep! :completion vertico)

;;;###autoload
(defun +embark-find-file (&rest _)
  (+search-minibuf-quit-and-run (call-interactively 'find-file)))

;;;###autoload
(defun +embark-search-file-cwd (&rest _)
  "Perform a recursive file search from the current directory."
  (+search-minibuf-quit-and-run (doom-project-find-file default-directory)))

;;;###autoload
(defun +embark-search-file-other-dir (&optional input)
  "Perform a recursive file search from the current directory."
  (let* ((projectile-project-root nil)
         (disabled-command-function nil)
         (default-directory (expand-file-name (read-directory-name "Search directory: "))))
    (embark--quit-and-run
     (lambda ()
       (minibuffer-with-setup-hook
           (lambda ()
             (delete-minibuffer-contents)
             (insert input))
         (doom-project-find-file default-directory))))))

;;;###autoload
(defun +embark-search-file-other-project (&optional input)
  "Perform a recursive file search from the current directory."
  (let* ((projectile-project-root nil)
         (disabled-command-function nil)
         (default-directory
           (if-let (projects (projectile-relevant-known-projects))
               (completing-read "Search project: " projects nil t)
             (user-error "There are no known projects"))))
    (embark--quit-and-run
     (lambda ()
       (minibuffer-with-setup-hook
           (lambda ()
             (delete-minibuffer-contents)
             (insert input))
         (doom-project-find-file default-directory))))))

;;;###autoload
(defun +embark/grep-project ()
  (interactive)
  (+default/search-project))

;;;###autoload
(defun +embark-grep-other-cwd (&optional input)
  (let* ((projectile-project-root nil)
         (disabled-command-function nil)
         (default-directory (expand-file-name (read-directory-name "Search directory: "))))
    (setq this-command #'+embark-grep-other-cwd)
    (embark--quit-and-run #'+vertico/project-search nil input default-directory)))

;;;###autoload
(defun +embark-grep-other-project (&optional input)
  (let* ((projectile-project-root nil)
         (disabled-command-function nil)
         (default-directory
           (if-let (projects (projectile-relevant-known-projects))
               (completing-read "Search project: " projects nil t)
             (user-error "There are no known projects"))))
    (setq this-command #'+embark-grep-other-project)
    (embark--quit-and-run #'+vertico/project-search nil input default-directory)))

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
(defun snail--project-grep (&rest _)
  (declare (indent defun))
  (unless (executable-find "rg")
    (user-error "Couldn't find ripgrep in your PATH"))
  (setq deactivate-mark t)
  (let* ((project-root (or (doom-project-root) default-directory))
         (directory project-root)
         (consult-ripgrep-args
          (concat "rg "
                  "--null --line-buffered --color=never --max-columns=1000 "
                  "--path-separator /   --smart-case --no-heading --line-number "
                  "--hidden -g !.git -g !.svn -g !.hg "
                  " ."))
         (prompt "Search")
         (query (when (doom-region-active-p)
                      (regexp-quote (doom-thing-at-point-or-region))))
         (consult-async-split-style consult-async-split-style)
         (consult-async-split-styles-alist consult-async-split-styles-alist)
         (read-process-output-max (max read-process-output-max (* 1024 1024))))
    ;; Change the split style if the initial query contains the separator.
    (when query
      (cl-destructuring-bind (&key type separator initial)
          (consult--async-split-style)
        (pcase type
          (`separator
           (replace-regexp-in-string (regexp-quote (char-to-string separator))
                                     (concat "\\" (char-to-string separator))
                                     query t t))
          (`perl
           (when (string-match-p initial query)
             (setf (alist-get 'perlalt consult-async-split-styles-alist)
                   `(:initial ,(or (cl-loop for char in (list "%" "@" "!" "&" "/" ";")
                                            unless (string-match-p char query)
                                            return char)
                                   "%")
                     :type perl)
                   consult-async-split-style 'perlalt))))))
    ;; (consult--async-command #'consult--ripgrep-builder
    ;;   (consult--grep-format #'consult--ripgrep-builder)
    ;;   :file-handler t)
    ))

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
    :narrow (?r . "Recent")
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
  '(snail--source-buffer
    snail--source-project-file
    snail--source-recent-file
    ;; hiden
    consult--source-hidden-buffer
    ))

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
