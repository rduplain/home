;;;; feature.el --- Declare third-party packages for installation and setup.

;;;; Meta

;;; Usage: Declare packages with `feature'; finalize with `feature-setup'.

;;; On a clean Emacs installation, `feature-setup' does nothing by default
;;; (because the ~/.emacs.d/straight/ directory does not exist). Call
;;; `feature-bootstrap' to create the package directory and install packages.

;;; Use `feature-upgrade-all' to upgrade all packages.

;;; The feature.el module provides a single entry point `feature' to declare
;;; third-party packages, which are then install via `straight-use-package'
;;; with all straight.el interactions wrapped within `feature-setup', with
;;; design goals:
;;;
;;; * Provide a single entry point to declare third-party packages.
;;; * Provide declaration which is unaffected by package manager changes.
;;; * Assume intent is to have the latest version of a package, by default.
;;; * Accept a version attribute to pin a package to a specific version.
;;; * Load all packages eagerly to allow for immediate use, by default.
;;; * Do nothing on a clean installation, by default.
;;; * Defer and batch installation into a single operation.

;;; A "recipe" names a package for installation, and has layers of semantics. A
;;; "recipe" can refer to:
;;;
;;; * The argument to `package-install' provided by Emacs built-in
;;;   package.el. The straight.el project refers to this as a MELPA-style
;;;   package recipe. Formally, it is a `package-desc', which in practice is a
;;;   symbol name matching a third-party package name or a list with that
;;;   symbol as the first item in the list ("car") followed by a property list.
;;;
;;; * A straight.el recipe which is the result of a direct conversion of the
;;;   MELPA-style recipe.
;;;
;;; * A straight.el recipe plist with additional attributes added here within
;;;   feature.el.

;;; See: https://github.com/raxod502/straight.el

;;; The name "feature" intentionally overloads FEATURE of Emacs built-in
;;; `provide'; `feature' declares a third-party FEATURE.

;;; Note that, in the event of namespace collisions, a separate project
;;; `feature-mode' declares various `feature-*` functions.


;;;; Public API

(defvar feature-list nil
  "List of features declared with `feature'.")

(defvar feature-setup-hook nil
  "Hook run after `feature-setup' (only if package directory exists).")

(defvar feature-no-setup-hook nil
  "Hook run after `feature-setup' (only if package directory does NOT exist).")

(defcustom feature-default-load nil
  "If non-nil, `require' features upon installation to load them eagerly."
  :type 'boolean)

(defmacro feature (recipe &rest body)
  "Declare a third-party feature to load via package manager.

`body` is accepted for convenience, to eval on load of feature.
Alternatively, provide `body` to `with-eval-after-load'
separately using the symbol accepted by `require' to load the
respective package."
  (declare (indent 1))
  (let* ((feature-by-symbol? (pcase recipe
                               (`(quote ,(pred atom)) t)))
         (feature-symbol (if feature-by-symbol?
                             recipe
                           (list 'quote (eval `(car ,recipe))))))
    (if body
        `(progn
           (add-to-list 'feature-list ,recipe)
           (with-eval-after-load ,feature-symbol
             ,@body))
      `(add-to-list 'feature-list ,recipe))))

(defun feature-bootstrap ()
  "Create the package directory and run `feature-setup'.

Call this on a clean Emacs installation."
  (interactive)
  (make-directory (feature--package-directory) t)
  (straight-bootstrap)
  (feature-setup))

(defun feature-setup ()
  "Set up features declared with `feature'."
  (interactive)
  (if (file-directory-p (feature--package-directory))
      (progn
        (feature--install-all)
        (feature--load-all)
        (run-hooks 'feature-setup-hook)
        (message "Third-party packages are ready."))
    (progn
      (message "Call `feature-bootstrap' to install third-party packages.")
      (run-hooks 'feature-no-setup-hook))))

(defun feature-upgrade-all ()
  "Upgrade all third-party features and re-run `feature-setup'."
  (interactive)
  (feature--upgrade-all)
  (feature-setup))


;;;; Private API

(defun feature--install-all ()
  "Install all features in `feature-list' via `feature--install'."
  (mapcar 'feature--install (feature--list)))

(defun feature--install (recipe)
  "Install third-party package specified by feature."
  (straight--with-plist (feature--plist recipe)
      (straight-recipe melpa-style-recipe
                       feature-symbol local-repo package type version)
    (when version
      (straight-use-package melpa-style-recipe nil 'no-build)
      (let ((commit (straight-vc-get-commit type local-repo)))
        (straight-vc-check-out-commit straight-recipe version)
        (unless (string= commit (straight-vc-get-commit type local-repo))
          (straight-rebuild-package package))))
    (straight-use-package melpa-style-recipe)))

(defun feature--list ()
  "Iterate `feature-list'."
  (reverse feature-list))

(defun feature--load-all ()
  "Load all features in `feature-list' via `feature--load'."
  (mapcar 'feature--load (feature--list)))

(defun feature--load (recipe)
  "Load feature if recipe :load or `feature-default-load' is non-nil."
  (straight--with-plist (feature--plist recipe)
      (feature-symbol has-load? load)
    (when (or (and (not has-load?) feature-default-load)
              (and has-load? load))
      (message "Loading feature: %s" feature-symbol)
      (require feature-symbol))))

(defun feature--package-directory ()
  "Return path to package directory."
  (expand-file-name "straight" user-emacs-directory))

(defun feature--plist (recipe)
  "Return an internal representation of `feature' specification."
  (let* ((feature-symbol (if (listp recipe)
                             (car recipe)
                           recipe))
         (original-plist (when (listp recipe)
                           (cdr recipe)))

         (version (plist-get original-plist :version))
         (has-load? (when (plist-member original-plist :load) t))
         (load (plist-get original-plist :load))

         (melpa-style-recipe-plist nil) ; plist for melpa-style-recipe
         (melpa-style-recipe nil)       ; MELPA-style recipe (symbol or list)
         (straight-recipe nil))         ; straight.el converted recipe

    (while original-plist
      (let ((key (pop original-plist))
            (value (pop original-plist)))
        (unless (or (eq key :version) (eq key :load))
          (setq melpa-style-recipe-plist
                (plist-put melpa-style-recipe-plist key value)))))

    (setq melpa-style-recipe
          (if melpa-style-recipe-plist
              (cons feature-symbol melpa-style-recipe-plist)
            feature-symbol))

    (setq straight-recipe (straight--convert-recipe
                           (or (straight--get-overridden-recipe feature-symbol)
                               melpa-style-recipe)))

    (feature--plist-merge (list :feature-symbol feature-symbol
                                :melpa-style-recipe melpa-style-recipe
                                :straight-recipe straight-recipe
                                :has-load? has-load?
                                :load load
                                :version version)
                          straight-recipe)))

(defun feature--plist-merge (&rest plists)
  "Destructively merge plist values into single property list."
  (when plists
    (let ((result (car plists)))
      (dolist (plist (cdr plists) result)
        (while plist
          (setq result (plist-put result (pop plist) (pop plist))))))))

(defun feature--upgrade-all ()
  "Upgrade all features."
  (straight-pull-all))


;;;; Announce `feature'.

(provide 'feature)
