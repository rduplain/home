;;;; Declare third-party packages as features.

(defvar feature-list nil
  "List of features declared with `feature'.")

(defvar feature-setup-hook nil
  "Hook run after `feature-setup' (whether or not anything installed).")

(defmacro feature (feature &rest body)
  "Declare a feature to load via package manager.

`body` is accepted for convenience, to eval on load of feature.

If the feature is loaded with a recipe other than its feature
name, provide that recipe to `feature' alone, then provide `body`
to `with-eval-after-load'."
  (declare (indent 1))
  (let ((feature-by-name? (pcase feature
                            (`(quote ,(pred atom)) t))))
    (if (and body (not feature-by-name?))
        (error "Declare `feature' by name when providing body to eval: %S"
               feature)
      (if feature-by-name?
          `(progn
             (add-to-list 'feature-list ,feature)
             (require ,feature nil 'noerror)
             (with-eval-after-load ,feature
               ,@body))
        `(add-to-list 'feature-list ,feature)))))

(defun feature-install ()
  "Install features declared with `feature', if straight directory exists."
  (let ((straight-path (expand-file-name "straight" user-emacs-directory)))
    (if (file-directory-p straight-path)
        (straight-transaction
          (mapcar 'feature-install-recipe (reverse feature-list)))
      (message "Create dir in order to install packages: %s" straight-path))))

(defun feature-install-recipe (recipe)
  "Install package named in feature."
  (feature--with-recipe recipe (package recipe type version)
    (when version
      (straight-use-package recipe nil 'no-build)
      (straight-vc-check-out-commit type (symbol-name package) version))
    (straight-use-package recipe)))

(defun feature-setup ()
  "Set up features declared with `feature'."
  (feature-install)
  (run-hooks 'feature-setup-hook))

(defun feature--parse-recipe (recipe)
  "Parse high-level attributes of recipe, for convenient destructuring."
  (if (symbolp recipe)
      `(:package ,recipe
                 :recipe ,recipe)
    (let* ((package (car recipe))
           (recipe-plist (cdr recipe))
           (version (plist-get recipe-plist :version)))
      (straight--remq recipe-plist `(:version))
      `(:package ,package
                 :recipe ,(if recipe-plist
                              (cons package recipe-plist)
                            package)
                 :type ,(or (plist-get recipe-plist :type)
                            straight-default-vc)
                 :version ,version))))

(defmacro feature--with-recipe (recipe attrs &rest body)
  "Binding from parsed RECIPE the given ATTRS, eval and return BODY."
  (declare (indent 2) (debug (form sexp body)))
  `(straight--with-plist (feature--parse-recipe ,recipe) ,attrs ,@body))
