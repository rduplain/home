;;; Emacs configuration for Ron DuPlain.
;;;
;;; GNU Emacs 24.4+

;;; Meta

;; Configure variables.
;;
;; setq         - Set variable for all of Emacs.
;; setq-default - Set default variable, potentially overridden in local buffer.
;; add-to-list  - Insert a value into a list with a given (symbol) name.
;; fset         - Override a function with a given (symbol) name.
;; (function)   - Call a configuration function directly (without setq).

;; Edit .emacs in Emacs.
;;
;; C-h o RET    - View doc for symbol at cursor.


;;; Basics

;; Note: Packages are loaded in ~/.emacs.d/elpa, which is synced separately
;; between installations. Check this directory when initalization fails.

;; Use elpa package manager and load all installed packages.
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

;; Do not have an initial major mode. Set *scratch* to Fundamental, not Lisp.
(setq initial-major-mode '(lambda () nil))

;; Disable useless decorations.
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Disable the menu bar.
(menu-bar-mode -1)

;; Add a final newline automatically on save.
(setq-default require-final-newline t)

;; Accept y or n when presented with yes or no.
(fset 'yes-or-no-p 'y-or-n-p)

;; Disable beeps.
(setq ring-bell-function 'ignore)

;; Disable automatic backups, i.e. filename~ files.
(setq-default backup-inhibited t)

;; Disable tramp.
(setq tramp-mode nil)

;; Always use syntax highlighting.
(global-font-lock-mode 1)

;; Always show parentheses, in every mode.
(show-paren-mode t)
(setq show-paren-delay 0)

;; Inform Emacs of a dark color background.
;; Essential for modes (e.g. rst-mode) which highlight sections & blocks.
(setq frame-background-mode 'dark)

;; Show the column number in addition to the line number.
(setq-default column-number-mode t)

;; Don't just show me buffers, interact!
(global-set-key "\C-x\C-b" 'electric-buffer-list)

;; Indent only with spaces (default 4), never tabs.
(setq-default standard-indent 4)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Disable auto-fill-mode by default.
(auto-fill-mode -1)

;; Fill columns at 80 characters.
(setq-default fill-column 79)

;; Truncate long lines on partial windows.
(setq truncate-partial-width-windows t)

;; Remove trailing whitespace before saving files.
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;;; Extensions

;; Use magit for git interactions.
(global-set-key "\C-xg" 'magit-status)
(add-hook 'magit-log-edit-mode-hook 'configure-commit-buffer)

;; Use bookmark+.
(require 'bookmark+)

;; Extend dired.
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")))

; Ignore uninteresting files.
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-omit-mode 1)))

; Simplify declaration of patterns to omit in dired.
(defmacro add-to-dired-omit (&rest expressions)
  "Append to dired-x regular expression for dired-omit-mode"
  `(setq dired-omit-files
         (concat ,dired-omit-files
                 "\\|"
                 (mapconcat 'identity ',expressions "\\|"))))

; Archive files are uninteresting in emacs.
(add-to-dired-omit "\\.zip$")
(add-to-dired-omit "\\.tar\\.gz$" "\\.tgz$")
(add-to-dired-omit "\\.tar\\.bz$" "\\.tar\\.bz2$" "\\.tbz$" "\\.tbz2$")

; Swap files from vi/vim are uninteresting.
(add-to-dired-omit "\\.sw[op]$")

; Build output directories are uninteresting (most of the time).
(add-to-dired-omit "^dist$" "^out$" "^target$")

; Data and database files are uninteresting in emacs.
(add-to-dired-omit "\\.dat$" "\\.sqlite3?$")
(add-to-dired-omit "\\.db$" "\\.db-journal$")

; Jupyter/IPython Notebook checkpoints are uninteresting.
(add-to-dired-omit "^\\.ipynb_checkpoints$")

; Mac OS X clutter is uninteresting.
(add-to-dired-omit "^\\.DS_Store$" "^__MACOSX$")

; Emacs project files (e.g. desktop) are unintersting in dired.
(add-to-dired-omit "^\\.emacs.*$")

; Empty target files from make (to record events) are uninteresting.
(add-to-dired-omit "^\\.ts-.*$" "^\\..*-install")

; Log files are interesting, but not all the time.
(add-to-dired-omit "\\.log$")

; Version control databases are interesting, but not all the time.
(add-to-dired-omit "^\\.bzr$" "^_darcs$" "^\\.git$" "^\\.hg$")


;;; Fixes

;; Some terminals (incl. PuTTY) send [select] on home keypress.
(define-key global-map [select] 'end-of-line)


;;; Suspend vs. Shell

;; Disable suspend.
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; Bind to shell instead of suspending.
(defun interactive-shell (&optional shell)
  "Run ansi-term with the given shell, default $SHELL, falling back to bash."
  (interactive)
  (let ((shell (or shell
                   (getenv "SHELL")
                   "/bin/bash")))
    (ansi-term shell)))

(global-set-key (kbd "C-z") 'interactive-shell)


;;; Convenient Functions

;; Insert the date with F5, formatted just like the UNIX date command.
(defun insert-date (&optional insert-date-format)
  "Insert the current date formatted to the given argument."
  (interactive)
  (let ((insert-date-format (or insert-date-format
                                "%a %b %d %T %Z %Y\n")))
    (insert (format-time-string insert-date-format))))

(global-set-key [f5] 'insert-date)

;; Enlarge windows with ease with Control-6.
(global-set-key [?\C-^] 'enlarge-window)

;; Change line endings between Unix and DOS.
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

(fset 'todos 'unix2dos)
(fset 'fromdos 'dos2unix)


;;; Modes - Utilities to Support Configuration

(require 'subr-x)

(defun dominating-file (filename)
  "Provide filepath of dominating file, or nil, walking up directory tree."
  (let ((directory (locate-dominating-file "." filename)))
    (when directory
      (concat (file-name-as-directory directory) filename))))

(defun dominating-directory (filename)
  "Provide directory of dominating file, or ., walking up directory tree."
  (or (locate-dominating-file "." filename) "."))

(defun file-string (filepath)
  "Read the contents of a file and return as a string."
  (when filepath
    (with-temp-buffer
      (insert-file-contents filepath)
      (buffer-string))))

(defun filter (pred coll)
  "Return list of items in coll for which pred returns true."
  (let ((null-item (gensym)))
    (delete null-item
            (mapcar (lambda (item)
                      (if (funcall pred item)
                          item
                        null-item))
                    coll))))

(defun find-buffer (regexp)
  "Find existing buffer with name matching regular expression"
  (car
   (filter 'identity
           (mapcar (lambda (buffer)
                     (when (string-match regexp (buffer-name buffer))
                       buffer))
                   (buffer-list)))))

(defun on-keyword (fn filepath &rest keywords)
  "Apply fn to keywords found when searching keywords in a given file."
  (when filepath
    (let* ((pattern (string-join keywords "\\|"))
           (command (format "grep -e '%s' -o %s" pattern filepath))
           (result (string-trim (shell-command-to-string command))))
      (unless (string= "" result)
        (funcall fn (split-string result "\n"))))))

(defun project-path-from (dominating-filename path)
  "Provide filepath within project, with directory of dominating-file as root."
  (let ((directory (locate-dominating-file "." dominating-filename)))
    (when directory
      (concat (file-name-as-directory directory) path))))

(defun sniff (filepath &rest keywords)
  "Return the first keyword found when searching keywords in a given file."
  (apply 'on-keyword 'car filepath keywords))

(defun tcp-listening (port)
  "Return non-nil if a process is listening on localhost for given TCP port."
  (let ((proc (ignore-errors
                (open-network-stream "tcp-listening" nil "localhost" port))))
    (when proc
      (process-send-eof proc)
      t)))


;;; Modes - General Purpose

;; Set the default mode to Text.
(setq default-major-mode 'text-mode)

;; Interactively Do Things, with fuzzy matching enabled.
(require 'ido)
(setq ido-mode 'both) ; both file and buffer.
(setq ido-enable-flex-matching t)
(ido-mode 1)

;; Redo
(require 'redo+)
(global-set-key (kbd "C-/") 'undo)
(global-set-key (kbd "C-\\") 'redo)

;; Winner, undo and redo window configuration changes.
(winner-mode 1)
(global-set-key (kbd "C-x /") 'winner-undo)
(global-set-key (kbd "C-x \\") 'winner-redo)

;; Flyspell, on-the-fly spellcheck.

; Keep quiet.
(setq flyspell-issue-welcome-flag nil)
(setq flyspell-issue-message-flag nil)

; Sort by likelihood, not alphabetically.
(setq flyspell-sort-corrections nil)

; Highlight only when near cursor.
(setq flyspell-persistent-highlight nil)

; Override default key binding.
(setq flyspell-use-meta-tab nil)
(global-unset-key [?\M-s])
(setq flyspell-auto-correct-binding [?\M-s])

;; Yet Another Snippet system, for code/text snippets.
(require 'yasnippet)
(setq yas-snippet-dirs (cons "~/.emacs.d/snippets" yas-snippet-dirs))
(yas-global-mode 1)

;; Drag stuff.
(require 'drag-stuff)
(drag-stuff-global-mode 1)
(defvar drag-stuff-mode-map (make-sparse-keymap)
  "Keymap for `drag-stuff-mode'.")
(define-key drag-stuff-mode-map (kbd "M-p") 'drag-stuff-up)
(define-key drag-stuff-mode-map (kbd "M-n") 'drag-stuff-down)

;; Compilation: compile/recompile when using a Makefile.

; Scroll the compilation buffer with new output.
(setq compilation-scroll-output t)

; Do not ask for the make command to run. This is set with the hook below.
(setq compilation-read-command nil)

; Support a configurable `make` recipe, global for all buffers.
(defvar compile-command-recipe nil "make recipe")

; Define a function to interactively configure `make` recipe.
(defun compile-set-command-recipe (recipe)
  "Interactively set the value of compile-command-recipe for all buffers."
  (interactive "smake recipe: ")
  (setq compile-command-recipe recipe))

; Set a global key to set `make` recipe.
(global-set-key (kbd "C-x M-a") 'compile-set-command-recipe)

; Define wrapper function for `compile` to set its command just in time.
(defun compile-with-config ()
  "Set a compile command just in time and run Emacs `compile`.

  Run the Emacs `compile` command with `make` against the nearest
  Makefile using the recipe interactively set with `C-x M-a`."
  (interactive)
  (let ((command (format "make -C %s %s"
                         (dominating-directory "Makefile")
                         (or compile-command-recipe ""))))
    (compile command)))

; Set a global key for compilation, in all modes.
(global-set-key (kbd "C-x C-a") 'compile-with-config)


;;; Modes - Configure a REPL based on project files.

; Define zero-configuration command to run Emacs-integrated REPL, if available.
(defun run-repl ()
  "Run an Emacs-integrated REPL, if available, based on project files."
  (interactive)
  (cond

   ((dominating-file "shadow-cljs.edn")
    (run-repl-rebind-to-cider)
    (setq cider-preferred-build-tool 'shadow-cljs)
    (setq cider-shadow-cljs-global-options "--force-spawn")
    (let ((keywords '(":node-script")))
      (pcase (apply 'sniff (dominating-file "shadow-cljs.edn") keywords)

        (":node-script"
         (unless repl-hook-added?
           (add-hook 'nrepl-connected-hook 'on-shadow-cljs-node-repl)
           (setq repl-hook-added? t))
         (cider-jack-in `()))

        (_ (error "No REPL. Update ~/.emacs for this shadow-cljs.edn.")))))

   ((dominating-file ".nrepl-port")
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

   ((dominating-file "build.boot")
    (run-repl-rebind-to-cider)
    (cider-jack-in `()))

   ((dominating-file "project.clj")
    (run-repl-rebind-to-cider)
    (cider-jack-in `()))

   (t (error "No REPL. Update ~/.emacs to support this project."))))

; Track whether hook was added to nrepl-connected-hook.
(unless (boundp 'repl-hook-added?)
  (setq repl-hook-added? nil))

(defun on-shadow-cljs-node-repl ()
  "Hook to run on newly created shadow-cljs REPL."
  (message "Starting node-repl ...")
  (cider-interactive-eval
   "(do (require '[shadow.cljs.devtools.api])
        (shadow.cljs.devtools.api/node-repl :app))"
   ;; Then:
   (lambda (response)
     (nrepl-dbind-response response (status)
       (when (member "done" status)
         ;; Load & eval all clj/cljc files, to ensure that any macros defined
         ;; therein are available before evaluating cljs files.
         (message "Loading project clj/cljc files ...")
         (when-let
             ((src-path (project-path-from "shadow-cljs.edn" "src")))
           (cider-load-all-files-clj-cljc src-path)))))))

; Dynamically reconfigure REPL key binding.
(setq run-repl-kbd-str "C-x C-z")
(setq run-repl-kbd (kbd run-repl-kbd-str))
(setq run-repl-reset-kbd (kbd "C-x M-z"))

(defun run-repl-rebind-default ()
  (global-set-key run-repl-kbd 'run-repl))

(defun run-repl-rebind-to-cider ()
  (global-set-key run-repl-kbd 'cider-switch-to-repl-buffer))

; Set a global key for REPL, in all modes.
(run-repl-rebind-default)

; Provide an alternate key binding to restore default `run-repl` key binding.
(defun run-repl-reset ()
  "Reset 'run-repl key binding (since it rebinds itself to the new REPL)."
  (interactive)
  (run-repl-rebind-default)
  (message "Reset 'run-repl key binding: %s" run-repl-kbd-str))

(global-set-key run-repl-reset-kbd 'run-repl-reset)


;;; Modes - Programming Languages, Formats, & Frameworks
;;;
;;; Many language modes just work and are omitted here.

;; C
(add-hook 'c-mode-hook 'flyspell-prog-mode)

;; C++
(add-hook 'c++-mode-hook 'flyspell-prog-mode)

;; CSS
(add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))

;; Conf
(add-hook 'conf-mode-hook 'flyspell-prog-mode)

;; Clojure
(add-hook 'clojure-mode-hook 'flyspell-prog-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
; Skip :user section of ~/.lein/profiles.clj when using cider-jack-in.
(setq cider-lein-parameters
      "with-profile -user repl :headless :host localhost")

(defun cider-load-all-files-clj-cljc (directory)
  "Load all files in DIRECTORY (recursively). Added to include .cljc files.

  See `cider-load-all-files'."
  (interactive "DLoad files beneath directory: ")
  (mapcar #'cider-load-file
          ;; cider-load-all-files as of CIDER 0.19.0 has ".clj$".
          (directory-files-recursively directory "\\.cljc?$")))

(defun cider-load-all-files-cljs (directory)
  "Load all .cljs files in DIRECTORY (recursively). Added for .cljs option.

  See `cider-load-all-files'."
  (interactive "DLoad files beneath directory: ")
  (mapcar #'cider-load-file
          (directory-files-recursively directory "\\.cljs?$")))

(defun clear-cider-repl ()
  "Clear CIDER REPL buffer, callable from any buffer."
  (interactive)
  (when-let ((cider-repl
              (find-buffer "^\\*cider-repl.*$")))
    (with-current-buffer cider-repl
      (ignore-errors
        (cider-repl-clear-buffer)))))

; Set key binding to clear the CIDER REPL from a Clojure buffer.
(add-hook 'clojure-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c l") 'clear-cider-repl)))

; Set key binding to clear the CIDER REPL from the CIDER REPL.
(add-hook 'cider-repl-mode-hook
          '(lambda ()
             (define-key cider-repl-mode-map
               (kbd "C-c l")
               'cider-repl-clear-buffer)))

(add-to-dired-omit "^\\.cpcache$" "^\\.nrepl-port$")
(add-to-dired-omit "^\\.cljs_node_repl$" "^\\.shadow-cljs$")

;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;; Go
(add-hook 'go-mode-hook 'flyspell-prog-mode)
(add-hook 'go-mode-hook 'yas/minor-mode)

;; Haskell
(add-hook 'haskell-mode-hook 'flyspell-prog-mode)

;; HTML
(add-to-list 'auto-mode-alist '("\\.html$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.vue$" . html-mode))
(add-hook 'html-mode-hook 'flyspell-prog-mode)
(add-hook 'html-mode-hook '(lambda ()
                             (auto-fill-mode -1)
                             (setq-local indent-tabs-mode nil)
                             (setq-local standard-indent 2)
                             (setq-local tab-width 2)))

;; JavaScript
(defun javascript-settings ()
  (auto-fill-mode -1)
  (setq-local indent-tabs-mode nil)
  (setq-local standard-indent 2)
  (setq-local js-indent-level 2)
  (setq-local tab-width 2))

(add-hook 'js-mode-hook 'javascript-settings)
(add-hook 'json-mode-hook 'javascript-settings)

(add-to-dired-omit "^node_modules$" "^package-lock\\.json$")

;; Java
(add-hook 'java-mode-hook 'flyspell-prog-mode)

;; LaTeX
(add-to-list 'auto-mode-alist '("\\.latex$" . latex-mode))

;; Markdown
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; OCaml
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el" 'noerror)


;; Python
(add-hook 'python-mode-hook 'flyspell-prog-mode)
(add-to-dired-omit "\\.egg$" "\\.egg-info$")
(add-to-dired-omit "^\\.coverage$" "^\\.tox$")
(add-to-dired-omit "\\.pyc$" "^__pycache__$")

;; R
(add-to-list 'auto-mode-alist '("\\.R$" . R-mode))
(add-to-list 'auto-mode-alist '("\\.r$" . R-mode))
(add-hook 'ess-mode-hook 'flyspell-prog-mode)

;; ReStructuredText (rst)
(add-to-list 'auto-mode-alist '("\\.rst.in$" . rst-mode))
(add-hook 'rst-mode-hook '(lambda () (auto-fill-mode nil)))

;; Ruby
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.simplecov$" . ruby-mode))
(add-hook 'ruby-mode-hook 'flyspell-prog-mode)
(add-hook 'ruby-mode-hook '(lambda ()
                             (setq-local standard-indent 2)))

;; Shell
(add-to-list 'auto-mode-alist '("\\.bats$" . sh-mode))
(add-hook 'sh-mode-hook 'flyspell-prog-mode)

;; Text
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook
          '(lambda ()
             ; Handle git commit message editing.
             (when (string= "COMMIT_EDITMSG" (buffer-name))
               (auto-fill-mode 1)
               (ruler-mode 1)
               (setq-local fill-column 72)
               (setq-local goal-column nil)
               (setq-local comment-column 50))))

;; Apache Thrift
(add-to-list 'auto-mode-alist '("\\.thrift$" . thrift-mode))

;; XML
(add-hook 'xml-mode-hook 'flyspell-prog-mode)

;; X
(add-to-list 'auto-mode-alist '("\\.xrdb$" . xrdb-mode))


;;; M-x customize

;; Manage custom-set-variables and custom-set-faces in a separate file.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
