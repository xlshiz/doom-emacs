;;; editor/search/autoload/become.el -*- lexical-binding: t; -*-
;;;###if (modulep! :completion vertico)

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
(defun +embark-clean-input(input)
  (if (string= (substring input 0 1) "#")
      (substring input 1)
    input))

;;;###autoload
(defun +embark/grep-project ()
  (interactive)
  (+default/search-project))

;;;###autoload
(defun +embark-grep-buffer (&optional input)
  (interactive)
  (embark--quit-and-run #'consult-line (+embark-clean-input input)))

;;;###autoload
(defun +embark-grep-other-cwd (&optional input)
  (let* ((projectile-project-root nil)
         (disabled-command-function nil)
         (default-directory (expand-file-name (read-directory-name "Search directory: "))))
    (setq this-command #'+embark-grep-other-cwd)
    (embark--quit-and-run #'+vertico/project-search nil (+embark-clean-input input) default-directory)))

;;;###autoload
(defun +embark-grep-other-project (&optional input)
  (let* ((projectile-project-root nil)
         (disabled-command-function nil)
         (default-directory
           (if-let (projects (projectile-relevant-known-projects))
               (completing-read "Search project: " projects nil t)
             (user-error "There are no known projects"))))
    (setq this-command #'+embark-grep-other-project)
    (embark--quit-and-run #'+vertico/project-search nil (+embark-clean-input input) default-directory)))
