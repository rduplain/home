;;;; Emacs configuration for R. DuPlain.
;;;;
;;;; GNU Emacs 26.1+

;;;; Meta

;;; Configure variables.
;;;
;;; (function)   - Call a configuration function directly.
;;; add-to-list  - For config variables which are lists, insert value at head.
;;; fset         - Set, alias, or override a function.
;;; load         - Load a module (a .el file) found on the `load-path'.
;;; setq         - Set variable for all of Emacs.
;;; setq-default - Set default variable, able to be overridden in local buffer.

;;; Edit .emacs in Emacs.
;;;
;;; C-h o RET    - View doc for symbol at cursor.


;;;; User Emacs Directory
(defun .emacs.d (file)
  "Expand to ~/.emacs.d/FILE."
  (expand-file-name file user-emacs-directory))


;;;; Load Configuration
(setq load-prefer-newer t)

(add-to-list 'load-path (.emacs.d "user"))

(load (.emacs.d "bytecomp-user"))

(byte-compile-quietly
 (byte-recompile-directory (.emacs.d "user") 0))


;;;; User-Defined Functions / Macros
(load "functions")
(load "patch")

;;; Clean up earlier byte-compilation.
(kill-buffer (find-buffer "\\*Compile-Log\\*"))


;;;; Packages
(load (.emacs.d "straight-init") 'noerror)
(load "feature")

;;; Clear recipe overrides in order to support reload of .emacs file.
(setq straight-recipe-overrides nil)

;;; Configure when straight.el looks for modifications.
;;;
;;; Look for package modifications with `straight-check-all' or when saving
;;; edits to files within ~/.emacs.d/straight/repos, but not at startup.
(setq straight-check-for-modifications '(find-when-checking check-on-save))


;;;; Basics

;;; Do not have an initial major mode. Set *scratch* to Fundamental, not Lisp.
(setq initial-major-mode '(lambda () nil))

;;; Disable useless decorations.
(setq inhibit-startup-message t
      initial-scratch-message nil)

;;; Disable the menu bar.
(menu-bar-mode -1)

;;; Add a final newline automatically on save.
(setq-default require-final-newline t)

;;; Accept y or n when presented with yes or no.
(fset 'yes-or-no-p (symbol-function 'y-or-n-p))

;;; Disable beeps.
(setq ring-bell-function 'ignore)

;;; Disable automatic backups, i.e. filename~ files.
(setq-default backup-inhibited t)

(setq auto-save-list-file-name nil
      auto-save-list-file-prefix nil)

;;; Disable tramp.
(setq tramp-mode nil)

;;; Always use syntax highlighting.
(global-font-lock-mode 1)

;;; Always show parentheses, in every mode.
(show-paren-mode t)
(setq show-paren-delay 0)

;;; Inform Emacs of a dark color background.
;;; Essential for modes (e.g. rst-mode) which highlight sections & blocks.
(setq frame-background-mode 'dark)

;;; Show the column number in addition to the line number.
(setq-default column-number-mode t)

;;; Don't just show me buffers, interact!
(global-set-key (kbd "C-x C-b") 'electric-buffer-list)

;; Set electric buffer key binding to sort by filename.
(add-hook 'electric-buffer-menu-mode-hook
          '(lambda ()
             (local-set-key (kbd "M-f")
                            '(lambda ()
                               (interactive)
                               ;; Tabular sort, where argument is column #.
                               (Buffer-menu-sort 3)))))

;; Set electric buffer key binding to kill buffer list (to reset sorting).
(add-hook 'electric-buffer-menu-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-M-f")
                            '(lambda ()
                               (interactive)
                               (kill-buffer
                                (find-buffer "\\*Buffer List\\*"))
                               (keyboard-quit)))))

;;; Indent only with spaces (default 4), never tabs.
(setq-default standard-indent 4
              tab-width 4
              indent-tabs-mode nil)

;;; Disable auto-fill-mode by default.
(auto-fill-mode -1)

;;; Fill columns at 80 characters.
(setq-default fill-column 79)

;;; Truncate long lines on partial windows.
(setq truncate-partial-width-windows t)

;;; Remove trailing whitespace before saving files.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Increase large file warning threshold.
(setq large-file-warning-threshold 50000000)

;;; Kill processes on exit without querying.
(setq confirm-kill-processes nil)


;;;; Fixes

;;; Some terminals (incl. PuTTY) send [select] on home keypress.
(define-key global-map [select] 'end-of-line)

;;; Always `vc-refresh-state' (follow symbolic links); accept the consequences.
(always-yes 'vc-refresh-state)


;;;; Extensions

;;; Use magit for git interactions.
(feature 'magit
  (load "magit-theme" 'noerror))

(global-set-key (kbd "C-x g") 'magit-status)

;; Support .homegit for tracking $HOME files.
;;
;; While changing magit's "global" configuration for a singular repository is
;; generally a bad idea, _this_ .emacs file supports many independent
;; concurrent emacs sessions. If Emacs is running from the $HOME directory,
;; then the intent is to edit $HOME files.
(unless (boundp 'homegit-magit-hook?)
  (setq homegit-magit-hook? nil))

(defun homegit-magit-hook ()
  (unless homegit-magit-hook?
    (let ((homegit-path (expand-file-name ".homegit")))
      (when (and (file-exists-p homegit-path)
                 (not (file-exists-p ".git")))
        (add-to-list 'magit-git-global-arguments
                     (format "--work-tree=%s"
                             (directory-file-name
                              (file-name-directory homegit-path))))
        (add-to-list 'magit-git-global-arguments
                     (format "--git-dir=%s" homegit-path))))
    (setq homegit-magit-hook? t)))

(with-eval-after-load 'magit
  (homegit-magit-hook))


;;; Extend dired.
(add-hook 'dired-load-hook
          (lambda ()
            (require 'dired-x nil 'noerror)))

;; Ignore uninteresting files.
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-omit-mode 1)))

;; Simplify declaration of patterns to omit in dired.
(defmacro add-to-dired-omit (&rest expressions)
  "Append to dired-x regular expression for dired-omit-mode"
  `(with-eval-after-load 'dired-x
     (setq dired-omit-files
           (concat ,'dired-omit-files
                   "\\|"
                   (mapconcat 'identity ',expressions "\\|")))))

;; Omit uninteresting files in dired.
;;                 ;; Archive files.
(add-to-dired-omit "\\.zip$"
                   "\\.tar\\.gz$" "\\.tgz$"
                   "\\.tar\\.bz$" "\\.tar\\.bz2$" "\\.tbz$" "\\.tbz2$"

                   ;; Swap files from vi/vim.
                   "\\.sw[op]$"

                   ;; Build output directories.
                   "^dist$" "^out$" "^target$"

                   ;; Data and database files.
                   "\\.dat$" "\\.sqlite3?$"
                   "\\.db$" "\\.db-journal$"

                   ;; Jupyter/IPython Notebook checkpoints.
                   "^\\.ipynb_checkpoints$"

                   ;; Mac OS X clutter.
                   "^\\.DS_Store$" "^__MACOSX$"

                   ;; Emacs desktop files.
                   "^\\.emacs\\.desktop.*$"

                   ;; Empty target files from make (to record events).
                   "^\\.ts-.*$" "^\\..*-install"

                   ;; Log files.
                   "\\.log$"

                   ;; Version control databases.
                   "^\\.bzr$" "^_darcs$" "^\\.git$" "^\\.hg$")


;;; Use an Emacs-native paste bin.
(load "htmlize-theme")

(feature '(scpaste :load t)
  ;; Patch directly, as `advice' appears to have byte-compiled limitations.
  (unless (fboundp 'scpaste-footer-original)
    (fset 'scpaste-footer-original (symbol-function 'scpaste-footer)))
  (fset 'scpaste-footer (symbol-function 'scpaste-footer-custom)))

(setq scpaste-http-destination "https://paste.duplain.io"
      scpaste-scp-destination "paste.duplain.io:/srv/paste"
      scpaste-user-name "rduplain"
      scpaste-user-address "https://github.com/rduplain")

(setq scpaste-footer-style
      `(,(concat "font-family: " htmlize-theme-font ", monospace")
        "font-size: 13pt"
        "color: #999999"))

(defun scpaste-footer-wrapper (fn &rest args)
  "Wrap `scpaste-footer' to customize footer style."
  (replace-regexp-in-string "style='.* -moz-user-select"
                            (format "style='%s; -moz-user-select"
                                    (string-join scpaste-footer-style "; "))
                            (funcall fn)))

(defun scpaste-footer-custom (&rest args)
  "Drop-in replacement for `scpaste-footer' calling `scpaste-footer-original'."
  (scpaste-footer-wrapper 'scpaste-footer-original))


;;; Customize eshell.
(setq eshell-directory-name ""
      eshell-history-file-name nil
      eshell-last-dir-ring-file-name nil)


;;;; Suspend vs. Shell

;;; Disable suspend.
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;;; Bind to shell instead of suspending.
(defun interactive-shell (&optional shell)
  "Run ansi-term with the given shell, default $SHELL, falling back to bash."
  (interactive)
  (let ((shell (or shell
                   (getenv "SHELL")
                   "bash")))
    (ansi-term shell)))

(global-set-key (kbd "C-z") 'interactive-shell)


;;;; Convenient Functions

;;; Insert the date with F5, formatted just like the UNIX date command.
(defun insert-date (&optional insert-date-format)
  "Insert the current date formatted to the given argument."
  (interactive)
  (insert (format-time-string (or insert-date-format "%a %b %d %T %Z %Y\n"))))

(global-set-key (kbd "<f5>") 'insert-date)

;;; Enlarge windows with ease with Control-6.
(global-set-key (kbd "C-^") 'enlarge-window)

;;; Change line endings between Unix and DOS.
(defun dos2unix ()
  "Change line endings of current buffer from DOS to Unix."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defun unix2dos ()
  "Change line endings of current buffer from Unix to DOS."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))

(fset 'todos (symbol-function 'unix2dos))
(fset 'fromdos (symbol-function 'dos2unix))


;;;; Modes - General Purpose

;;; Set the default mode to Text.
(setq default-major-mode 'text-mode)

;;; Interactively Do Things, with fuzzy matching enabled.
(setq ido-mode 'both ;; both file and buffer.
      ido-enable-flex-matching t
      ido-enable-last-directory-history nil
      ido-enable-tramp-completion nil
      ido-record-commands nil
      ido-record-ftp-work-directories nil)
(ido-mode 1)

;;; Redo
(feature '(redo+ :load t))

(global-set-key (kbd "C-/") 'undo)
(global-set-key (kbd "C-\\") 'redo)

;;; Winner, undo and redo window configuration changes.
(winner-mode 1)
(global-set-key (kbd "C-x /") 'winner-undo)
(global-set-key (kbd "C-x \\") 'winner-redo)

;;; Ace Window, jump to windows with M-o and home row keys.
(feature 'ace-window)

(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "C-x M-o") 'ace-window)

(setq aw-background nil
      aw-ignore-current nil
      aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
      aw-minibuffer-flag nil)

(custom-set-faces
 `(aw-leading-char-face
   ((t (:background "black" :foreground "yellow" :weight bold))))
 `(aw-minibuffer-leading-char-face
   ((t (:inherit aw-leading-char-face)))))

;;; Flyspell, on-the-fly spellcheck.

;; Keep quiet.
(setq flyspell-issue-welcome-flag nil
      flyspell-issue-message-flag nil)

;; Sort by order corrections are found (approx likelihood), not alphabetically.
(setq flyspell-sort-corrections nil)

;; Highlight only when near cursor.
(setq flyspell-persistent-highlight nil)

;; Override default key binding.
(setq flyspell-use-meta-tab nil)
(global-unset-key [?\M-s])
(setq flyspell-auto-correct-binding [?\M-s])

;;; Yet Another Snippet system, for code/text snippets.
(feature 'yasnippet
  (yas-global-mode 1))

;;; Use company-mode to "complete anything."
(feature 'company
  (setq company-global-modes '(not csv-mode eshell-mode text-mode))
  (global-company-mode)
  ;; Disable completion of plain text by removing dabbrev backends.
  (delete 'company-dabbrev company-backends)
  (delete 'company-dabbrev-code company-backends))

(defun set-text-based-company-minimum-prefix-length ()
  "Reusable function to set company-mode minimum prefix length.

In the event that `company-mode' is applied to a text-based mode, set a
suitable minimum prefix as to avoid completing filenames on a single '/'."
  (setq-local company-minimum-prefix-length 3))


;;; Configure how many characters are needed before presenting completions.
(setq company-minimum-prefix-length 1)

;;; Configure margin around completions.
;;; A margin greater than 0 avoids text appearing next to each other.
(setq company-tooltip-margin 1)

;;; Start completion immediately upon `company-minimum-prefix-length'.
(setq company-idle-delay 0)

(setq company-selection-wrap-around t)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "TAB") 'company-complete-selection)
  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
  (define-key company-search-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-search-map (kbd "C-p") 'company-select-previous-or-abort)
  (define-key company-search-map (kbd "C-t") 'company-search-toggle-filtering)

  (load "company-theme" 'noerror))

;;; Narrow

;; Enable Narrow functions. Widen with `C-x n w`.
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;;; Drag stuff.
(feature '(drag-stuff :load t)
  (drag-stuff-global-mode 1)
  (defvar drag-stuff-mode-map (make-sparse-keymap)
    "Keymap for `drag-stuff-mode'.")
  (define-key drag-stuff-mode-map (kbd "M-p") 'drag-stuff-up)
  (define-key drag-stuff-mode-map (kbd "M-n") 'drag-stuff-down))

;;; Static Checking
(with-eval-after-load 'flymake
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

;;; Compilation: compile/recompile when using a Makefile.

;; Scroll the compilation buffer with new output.
(setq compilation-scroll-output t)

;; Do not ask for the make command to run. This is set with the hook below.
(setq compilation-read-command nil)

;; Always save modified buffers when compiling.
(setq compilation-ask-about-save nil)

;; Always kill a running compilation process before starting a new one.
(setq compilation-always-kill t)

;; Support a configurable `make` recipe, global for all buffers.
(defvar compile-command-recipe nil "make recipe")

;; Define a function to interactively configure `make` recipe.
(defun compile-set-command-recipe (recipe)
  "Interactively set the value of compile-command-recipe for all buffers."
  (interactive "smake recipe: ")
  (setq compile-command-recipe recipe))

;; Set a global key to set `make` recipe.
(global-set-key (kbd "C-x M-a") 'compile-set-command-recipe)

;; Define wrapper function for `compile` to set its command just in time.
(defun compile-with-config ()
  "Set a compile command just in time and run Emacs `compile`.

  Run the Emacs `compile` command with `make` against the nearest
  Makefile using the recipe interactively set with `C-x M-a`."
  (interactive)
  (let ((command (format "make -C %s %s"
                         (dominating-directory "Makefile")
                         (or compile-command-recipe ""))))
    (compile command)))

;; Set a global key for compilation, in all modes.
(global-set-key (kbd "C-x C-a") 'compile-with-config)


;;;; Modes - Language Server Protocol

;;; Language Server Protocol - lsp-mode
(feature '(lsp-mode :load t))
(feature 'company-lsp)

;; Provide API to check whether `lsp' supports the current major mode.
(defun lsp-mode-supported-p (mode)
  "Return t if mode is supported by `lsp'; call with `major-mode'."
  ;; Assume that mode is set based on the filename, as this lookup does not
  ;; implement the filename pattern check in `lsp-buffer-language'.
  (when (alist-get mode lsp-language-id-configuration)
    t))

;;; Language Server Protocol - eglot mode
(feature '(eglot :load t))

;; Provide API to check whether eglot supports the current major mode.
(defun eglot-supported-p (mode)
  "Return t if mode is supported by `eglot'; call with `major-mode'."
  (when (assoc mode eglot-server-programs
               (lambda (m1 m2)
                 "Lifted from eglot.el."
                 (cl-find
                  m2 (if (listp m1) m1 (list m1))
                  :test #'provided-mode-derived-p)))
    t))


;;;; Modes - Configure a REPL based on project files.

;;; Configure inferior Lisp modes.
(setq display-buffer-alist
      '(("\\*inf-.*\\*"
         (display-buffer-below-selected display-buffer-at-bottom)
         (inhibit-same-window . t)
         (window-height . 10))))

;;; Define zero-configuration command to run available Emacs-integrated REPL.
(defun run-repl ()
  "Run an Emacs-integrated REPL, if available, based on project files."
  (interactive)
  (cond

   ((dominating-file "shadow-cljs.edn")
    (run-repl-shadow-cljs))

   ((dominating-file ".nrepl-port")
    (run-repl-nrepl))

   ((dominating-file "build.boot")
    (run-repl-boot))

   ((dominating-file "project.clj")
    (run-repl-lein))

   ((lsp-mode-supported-p major-mode)
    (call-interactively 'lsp))

   ((eglot-supported-p major-mode)
    (call-interactively 'eglot))

   ((eq major-mode 'janet-mode)
    (inf-janet inf-janet-program))

   (t (error "No REPL. Update ~/.emacs to support this project."))))

;;; Dynamically reconfigure REPL key binding.
(setq run-repl-kbd-str "C-x C-z"
      run-repl-kbd (kbd run-repl-kbd-str)
      run-repl-reset-kbd (kbd "C-x M-z"))

;;; Set a global key for REPL, in all modes.
(defun run-repl-rebind-default ()
  (global-set-key run-repl-kbd 'run-repl))

(run-repl-rebind-default)

;;; Provide an alternate key binding to restore default `run-repl` key binding.
(defun run-repl-reset ()
  "Reset 'run-repl key binding (since it rebinds itself to the new REPL)."
  (interactive)
  (run-repl-rebind-default)
  (message "Reset 'run-repl key binding: %s" run-repl-kbd-str))

(global-set-key run-repl-reset-kbd 'run-repl-reset)


;;;; Modes - Programming Languages, Formats, & Frameworks
;;;;
;;;; Many language modes just work and are omitted here.

;;; AsciiDoc
(feature 'adoc-mode)
(add-to-list 'auto-mode-alist '("\\.asciidoc$" . adoc-mode))
(add-to-list 'auto-mode-alist '("\\.adoc$" . adoc-mode))

(add-hook 'adoc-mode-hook 'set-text-based-company-minimum-prefix-length)

;;; C
(add-hook 'c-mode-hook 'flyspell-prog-mode)

;;; C++
(add-hook 'c++-mode-hook 'flyspell-prog-mode)

;;; C#
(feature 'csharp-mode)

;;; CSS
(add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))

(feature 'less-mode)
(feature 'sass-mode)

;;; Conf
(add-hook 'conf-mode-hook 'flyspell-prog-mode)

;;; Clojure
(feature 'clojure-mode)
(feature 'inf-clojure)
(add-hook 'clojure-mode-hook 'flyspell-prog-mode)
(with-eval-after-load 'rainbow-delimiters
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))

(feature '(cider :version "v0.22.0"))

(defun run-repl-boot ()
  "Run an Emacs-integrated REPL with boot-clj."
  (interactive)
  (require 'cider)

  (run-repl-rebind-to-cider)
  (cider-jack-in '()))

(defun run-repl-lein ()
  "Run an Emacs-integrated REPL with leiningen."
  (interactive)
  (require 'cider)

  (run-repl-rebind-to-cider)
  (cider-jack-in '()))

(defun run-repl-nrepl ()
  "Run an Emacs-integrated REPL connecting with .nrepl-port file."
  (interactive)
  (require 'cider)

  (if (thread-first
          (dominating-file ".nrepl-port")
        (file-string)
        (string-to-number)
        (tcp-listening))
      (let ((host "localhost")
            (port (file-string (dominating-file ".nrepl-port"))))
        (run-repl-rebind-to-cider)
        (cider-connect `(:host ,host :port ,port)))
    (error "Unable to connect to nREPL server with .nrepl-port file.")))

(defun run-repl-shadow-cljs ()
  "Run an Emacs-integrated REPL with shadow-cljs."
  (interactive)
  (require 'cider)

  (run-repl-rebind-to-cider)
  (setq cider-preferred-build-tool 'shadow-cljs
        cider-shadow-cljs-global-options "--force-spawn")
  (let ((keywords '(":node-script")))
    (pcase (apply 'sniff (dominating-file "shadow-cljs.edn") keywords)

      (":node-script"
       (unless repl-hook-added?
         (cider-register-cljs-repl-type
          'shadow-cljs-node
          "(do (require '[shadow.cljs.devtools.api])
                 (shadow.cljs.devtools.api/node-repl :app))")
         (add-hook 'nrepl-connected-hook 'on-shadow-cljs-node-repl)
         (setq repl-hook-added? t))
       (cider-jack-in-cljs '(:cljs-repl-type shadow-cljs-node)))

      (_ (error "No REPL. Update ~/.emacs for this shadow-cljs.edn.")))))

;;; Track whether hook was added to nrepl-connected-hook.
(unless (boundp 'repl-hook-added?)
  (setq repl-hook-added? nil))

(defun on-shadow-cljs-node-repl ()
  "Hook to run on newly created shadow-cljs REPL."
  (message "Waiting for cljs repl ...")
  (cider-interactive-eval
   "()"
   ;; Then:
   (lambda (response)
     (nrepl-dbind-response response (status)
       (when (member "done" status)
         ;; Load & eval all clj/cljc files, to ensure that any macros defined
         ;; therein are available before evaluating cljs files.
         (message "Loading project clj/cljc files ...")
         (when-let
             ((src-path (project-path-from "shadow-cljs.edn" "src")))
           (cider-load-all-files src-path)))))))

(defun run-repl-rebind-to-cider ()
  (global-set-key run-repl-kbd 'cider-switch-to-repl-buffer))

;; Skip :user section of ~/.lein/profiles.clj when using cider-jack-in.
(setq cider-lein-parameters
      "with-profile -user repl :headless :host localhost")

(setq cider-offer-to-open-cljs-app-in-browser nil)

(defun clear-cider-repl ()
  "Clear CIDER REPL buffer, callable from any buffer."
  (interactive)
  (when-let ((cider-repl
              (find-buffer "^\\*cider-repl.*$")))
    (with-current-buffer cider-repl
      (ignore-errors
        (cider-repl-clear-buffer)))))

;; Set key binding to clear the CIDER REPL from a Clojure buffer.
(add-hook 'clojure-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c l") 'clear-cider-repl)))

;; Set key binding to clear the CIDER REPL from the CIDER REPL.
(add-hook 'cider-repl-mode-hook
          '(lambda ()
             (define-key cider-repl-mode-map
               (kbd "C-c l")
               'cider-repl-clear-buffer)))

(add-to-dired-omit "^\\.cpcache$" "^\\.nrepl-port$"
                   "^\\.cljs_node_repl$" "^\\.shadow-cljs$")

;;; Crontab
(feature 'crontab-mode)

;;; CSV
(feature 'csv-mode)

;;; Cucumber
(feature 'feature-mode)

;;; Docker
(feature 'dockerfile-mode)

;;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
(with-eval-after-load 'rainbow-delimiters
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

;;; Go (golang)
(feature 'go-mode)
(add-hook 'go-mode-hook 'flyspell-prog-mode)
(add-hook 'go-mode-hook 'yas/minor-mode)

;;; Haskell
(feature 'haskell-mode)
(add-hook 'haskell-mode-hook 'flyspell-prog-mode)

;;; HTML
(add-to-list 'auto-mode-alist '("\\.html$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.vue$" . html-mode))
(add-hook 'html-mode-hook 'flyspell-prog-mode)
(add-hook 'html-mode-hook '(lambda ()
                             (auto-fill-mode -1)
                             (setq-local indent-tabs-mode nil)
                             (setq-local standard-indent 2)
                             (setq-local tab-width 2)))

(feature 'multi-web-mode)

;;; Janet
(feature '(janet-mode :host github :repo "ALSchwalm/janet-mode"))
(feature '(inf-janet :host github :repo "rduplain/inf-janet"))

(add-hook 'janet-mode-hook 'inf-janet-minor-mode)
(setq inf-janet-program "janet"
      inf-janet-project-root-files '(".git"))

(add-hook 'inf-janet-mode-hook
          '(lambda ()
             (setq-local truncate-lines nil)
             (setq-local truncate-partial-width-windows nil)))

;;; JavaScript
(defun javascript-settings ()
  (auto-fill-mode -1)
  (setq-local indent-tabs-mode nil)
  (setq-local standard-indent 2)
  (setq-local js-indent-level 2)
  (setq-local tab-width 2))

(add-hook 'js-mode-hook 'javascript-settings)
(add-hook 'json-mode-hook 'javascript-settings)

(add-to-dired-omit "^node_modules$" "^package-lock\\.json$")

(feature 'coffee-mode)
(feature 'json-mode)
(feature 'jsx-mode)

;;; Java
(add-hook 'java-mode-hook 'flyspell-prog-mode)

;;; LaTeX
(add-to-list 'auto-mode-alist '("\\.latex$" . latex-mode))

;;; Lua
(feature 'lua-mode)

;;; Markdown
(feature 'markdown-mode)
(feature 'markdown-toc)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(add-hook 'markdown-mode-hook 'set-text-based-company-minimum-prefix-length)

;;; Meson
(feature 'meson-mode)

;;; OCaml / ReasonML
(require 'opam-user-setup (.emacs.d "opam-user-setup.el") 'noerror)

(feature 'tuareg)
(feature 'reason-mode)

(add-to-list 'auto-mode-alist '("^\\.ocamlinit$" . tuareg-mode))

;;; PHP
(feature 'php-mode)

;;; Python
(add-hook 'python-mode-hook 'flyspell-prog-mode)
(add-to-dired-omit "\\.egg$" "\\.egg-info$"
                   "^\\.coverage$" "^\\.tox$"
                   "\\.pyc$" "^__pycache__$")

(feature 'hy-mode)
(feature 'pydoc)

;;; R
(feature 'ess)

(add-to-list 'auto-mode-alist '("\\.R$" . R-mode))
(add-to-list 'auto-mode-alist '("\\.r$" . R-mode))
(add-hook 'ess-mode-hook 'flyspell-prog-mode)

;;; ReStructuredText (rst)
(add-to-list 'auto-mode-alist '("\\.rst.in$" . rst-mode))
(add-hook 'rst-mode-hook '(lambda ()
                            (auto-fill-mode nil)))

(add-hook 'rst-mode-hook 'set-text-based-company-minimum-prefix-length)

;;; Ruby
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.simplecov$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.irbrc$" . ruby-mode))
(add-hook 'ruby-mode-hook 'flyspell-prog-mode)
(add-hook 'ruby-mode-hook '(lambda ()
                             (setq-local standard-indent 2)))

;;; Scala
(feature 'scala-mode)

;;; Sed
(feature 'sed-mode)

;;; Shell
(add-to-list 'auto-mode-alist '("\\.bats$" . sh-mode))
(add-hook 'sh-mode-hook 'flyspell-prog-mode)

;;; Text
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook
          '(lambda ()
             ;; Configure git commit message editing.
             (when (string= "COMMIT_EDITMSG" (buffer-name))
               (auto-fill-mode 1)
               (ruler-mode 1)
               (setq-local fill-column 72)
               (setq-local goal-column nil)
               (setq-local comment-column 50))))

(add-hook 'text-mode-hook 'set-text-based-company-minimum-prefix-length)

;;; Thrift
(feature 'thrift)
(add-to-list 'auto-mode-alist '("\\.thrift$" . thrift-mode))

;;; XML
(add-hook 'xml-mode-hook 'flyspell-prog-mode)

;;; X
(add-to-list 'auto-mode-alist '("\\.xrdb$" . xrdb-mode))

;;; YAML
(feature 'yaml-mode)


;;;; M-x customize

;;; Manage custom-set-variables and custom-set-faces in a separate file.
(setq custom-file (.emacs.d "custom.el"))
(load custom-file 'noerror)


;;;; Finalize
(feature-setup)


;;;; Desktop - Save & Restore Sessions

(setq desktop-auto-save-timeout nil
      desktop-base-file-name ".emacs.desktop"
      desktop-load-locked-desktop t
      desktop-path '(".")
      desktop-save 'if-exists)

;;; Add hook to restore frameset within tty (not supported by default).
(setq desktop-restore-forces-onscreen nil)
(add-hook 'desktop-after-read-hook
          (lambda ()
            (frameset-restore
             desktop-saved-frameset
             :reuse-frames (eq desktop-restore-reuses-frames t)
             :cleanup-frames (not (eq desktop-restore-reuses-frames 'keep))
             :force-display desktop-restore-in-current-display
             :force-onscreen desktop-restore-forces-onscreen)))

;;; Ensure Emacs exits without `desktop-save' prompt; accept the consequences.
(always-yes 'desktop-save)

(desktop-save-mode 1)
