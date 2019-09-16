;;;; Configure `htmlize-buffer' to display terminal colors.

;; X11-style RGB table: ~/.emacs.d/user/rgb.txt
;; Note that this table is cached on `htmlize' load.
(setq htmlize-x-library-search-path
      `(,(expand-file-name "user" user-emacs-directory)))

(setq htmlize-use-rgb-txt t
      htmlize-face-overrides
      `(font-lock-builtin-face (:foreground "blue" :weight bold)
        font-lock-comment-delimiter-face "dark yellow"
        font-lock-comment-face "dark yellow"
        font-lock-constant-face "magenta"
        font-lock-doc-face "dark green"
        font-lock-function-name-face (:foreground "blue" :weight bold)
        font-lock-keyword-face (:foreground "cyan" :weight bold)
        font-lock-negation-char-face nil
        font-lock-preprocessor-face (:foreground "blue" :weight bold)
        font-lock-reference-face "magenta"
        font-lock-string-face "dark green"
        font-lock-type-face "dark green"
        font-lock-variable-name-face "dark yellow"
        font-lock-warning-face (:foreground "red" :weight bold)
        default (:foreground "white" :background "black")))
