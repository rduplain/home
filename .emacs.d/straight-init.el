;;;; Bootstrap straight.el package manager.

(defvar bootstrap-version)

;; This straight.el bootstrap only runs if its directory exists.
;;
;; Create ~/.emacs.d/straight/ in order to use third-party packages,
;; then restart Emacs.
(let ((straight-path (expand-file-name "straight" user-emacs-directory))
      (bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (when (file-directory-p straight-path)
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))
