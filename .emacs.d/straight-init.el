;;;; Bootstrap straight.el package manager.

(defvar bootstrap-version)

(defun straight-bootstrap ()
  "Bootstrap straight.el installation via straight-init.el."
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

;; This straight.el bootstrap only runs if the "straight" directory exists.
(let ((straight-path (expand-file-name "straight" user-emacs-directory)))
  (when (file-directory-p straight-path)
    (straight-bootstrap)))
