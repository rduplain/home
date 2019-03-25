;;;; Declare third-party packages as features.

(setq feature-list '())

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
  "Install features declared with `feature'."
  (straight-transaction
    (mapcar 'straight-use-package (reverse feature-list))))
