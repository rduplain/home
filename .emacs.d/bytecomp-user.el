;;;; Emacs byte compilation user functions.

(require 'cl)

(eval-and-compile
  (require 'bytecomp))

(defmacro byte-compile-quietly (&rest body)
  "Suppress byte compilation log while evaluating `body`."
  ;; Lifted from `straight--byte-compile-package' and adapted to macro.
  ;;
  ;; These two `let' forms try very, very hard to make
  ;; byte-compilation an invisible process.
  `(cl-letf (;; Prevent Emacs from asking the user to save all their
             ;; files before compiling.
             ((symbol-function #'save-some-buffers) #'ignore)
             ;; Further disable byte-compile log.
             ((symbol-function #'byte-compile-log-1) #'ignore)
             ((symbol-function #'byte-compile-log-file) #'ignore)
             ((symbol-function #'byte-compile-log-warning) #'ignore))
     (let (;; Suppress messages about byte-compilation progress.
           (byte-compile-verbose nil)
           ;; Suppress messages about byte-compilation warnings.
           (byte-compile-warnings nil)
           ;; Suppress the remaining messages.
           (inhibit-message t)
           (message-log-max nil))
       ,@body)))
