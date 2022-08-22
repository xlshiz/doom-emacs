;;; editor/format/autoload/evil.el -*- lexical-binding: t; -*-
;;;###if (modulep! :editor evil)

;;;###autoload (autoload '+format:region "tools/format/autoload/evil" nil t)
(evil-define-operator +format:region (beg end)
  "Evil ex interface to `+format/region'."
  (interactive "<r>")
  (+format/region beg end))
