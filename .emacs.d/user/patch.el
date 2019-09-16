;;;; When no config is available, patch functions to force sensible defaults.

;;; Prepare to patch functions which ask questions despite their configuration.
(unless (fboundp 'yes-or-no-p-original)
  (fset 'yes-or-no-p-original (symbol-function 'yes-or-no-p)))

(defmacro -apply-using-yes-or-no-as (default)
  "Because resulting function is anonymous, it cannot be `advice-remove'd."
  `(lambda (fn &rest args)
     "Patched to skip all invocations of `yes-or-no-p'."
     (unwind-protect
         (progn
           (fset 'yes-or-no-p '(lambda (&rest args) ,default))
           (apply fn args))
       (fset 'yes-or-no-p (symbol-function 'yes-or-no-p-original)))))

(defmacro always-yes (fn)
  `(advice-add ,fn :around (-apply-using-yes-or-no-as t)))

(defmacro always-no (fn)
  `(advice-add ,fn :around (-apply-using-yes-or-no-as nil)))

(defun patch-function (&rest args)
  "Replace function :fn with function :prefer, saving original to :original.

After patching, calls to the symbol of :fn will instead call that
of :prefer with the original function value of :fn's symbol saved
to the symbol of :original.

Provide symbol values in a plist:

    (swap-function :fn 'function-to-wrap
                   :prefer 'my-function
                   :original 'function-to-wrap-original)

After patching, calls to `function-to-wrap` will instead call
`my-function` with the original function value of
`function-to-wrap` saved to `function-to-wrap-original`.

The :original symbol is only set if it is not previously bound,
as to make the patch idempotent (allowing for repeat calls)."
  (let ((fn-symbol (plist-get args :fn))
        (prefer-symbol (plist-get args :prefer))
        (original-symbol (plist-get args :original)))

    (unless (and fn-symbol prefer-symbol original-symbol)
      (signal 'wrong-type-argument
              (cons "Call with :fn, :prefer, :original:" args)))

    (unless (fboundp original-symbol)
      (fset original-symbol (symbol-function fn-symbol)))

    (fset fn-symbol (symbol-function prefer-symbol))))
