;;;; When no config is available, patch functions to force sensible defaults.

;;; Prepare to patch functions which ask questions despite their configuration.
(unless (boundp 'original-yes-or-no-p)
  (fset 'original-yes-or-no-p (symbol-function 'yes-or-no-p)))

(defmacro -apply-using-yes-or-no-as (default)
  "Because resulting function is anonymous, it cannot be `advice-remove'd."
  `(lambda (fn &rest args)
     "Patched to skip all invocations of `yes-or-no-p'."
     (unwind-protect
         (progn
           (fset 'yes-or-no-p '(lambda (&rest args) ,default))
           (apply fn args))
       (fset 'yes-or-no-p (symbol-function 'original-yes-or-no-p)))))

(defmacro always-yes (fn)
  `(advice-add ,fn :around (-apply-using-yes-or-no-as t)))

(defmacro always-no (fn)
  `(advice-add ,fn :around (-apply-using-yes-or-no-as nil)))
