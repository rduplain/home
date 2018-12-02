;;; Emacs configuration for Ron DuPlain.
;;;
;;; GNU Emacs 24+

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

(require 'cl)

;; Note: Packages are loaded in ~/.emacs.d/elpa, which is synced on new and
;; existing installations. Check this directory when initalization fails.

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
(require 'magit)
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

; Build output directories are uninteresting.
(add-to-dired-omit "^dist$")

; Data and database files are uninteresting in emacs.
(add-to-dired-omit "\\.dat$" "\\.sqlite3?$")
(add-to-dired-omit "\\.db$" "\\.db-journal$")

; TODO: WIP, move these to language area below.

; Python "egg" files are uninteresting.
(add-to-dired-omit "\\.egg$" "\\.egg-info$")

; Python test tool files are uninteresting.
(add-to-dired-omit "^\\.coverage$" "^\\.tox$")

; Python cache directories are uninteresting.
(add-to-dired-omit "^__pycache__$")

; Rope project directories are uninteresting.
(add-to-dired-omit "^\\.ropeproject$")

; IPython Notebook checkpoints are uninteresting.
(add-to-dired-omit "^\\.ipynb_checkpoints$")

; Mac OS X clutter is uninteresting.
(add-to-dired-omit "^\\.DS_Store$" "^__MACOSX$")

; Emacs project files (e.g. desktop) are unintersting in dired.
(add-to-dired-omit "^\\.emacs.*$")

; Empty target files from make (to record events) are uninteresting.
(add-to-dired-omit "^\\.ts-.*$")

; Log files are interesting, but not all the time.
(add-to-dired-omit "\\.log$")


;;; Fixes

;; Some terminals (incl. PuTTY) send [select] on home keypress.
(define-key global-map [select] 'end-of-line)


;;; Suspend vs. Shell

;; Disable suspend.
(global-unset-key (kbd "C-z"))

;; Bind to shell instead of suspending.
(defun interactive-shell (&optional shell)
  "Run ansi-term with the given shell, default bash."
  (interactive)
  (unless shell (setq shell "/bin/bash"))
  (ansi-term shell))

(global-set-key (kbd "C-z") 'interactive-shell)


;;; Convenient Functions

;; Insert the date with F5, formatted just like the UNIX date command.
(defun insert-date (&optional insert-date-format)
  "Insert the current date formatted to the given argument."
  (interactive)
  (unless insert-date-format (setq insert-date-format "%a %b %d %T %Z %Y\n"))
  (insert (format-time-string insert-date-format)))

(global-set-key [f5] 'insert-date)

;; Enlarge windows with ease with Control-6.
(global-set-key [?\C-^] 'enlarge-window)

;; Change line endings between Unix and DOS.
(defun dos2unix ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defun unix2dos ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))

(fset 'todos 'unix2dos)
(fset 'fromdos 'dos2unix)


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

;; Flymake, on-the-fly compilation & static analysis.
(setq flymake-no-changes-timeout nil)
(setq flymake-start-syntax-check-on-find-file -1)

;; Flyspell, on-the-fly spellcheck.

; Keep quiet.
(setq flyspell-issue-welcome-flag nil)
(setq flyspell-issue-message-flag nil)

; Sort by likelihood, not alphabetically.
(setq flyspell-sort-corrections nil)

; Highlight only when near cursor.
(setq flyspell-persistent-highlight nil)

; Override default key-binding.
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

;;; Programming Tools

;; Compilation: compile/recompile - great for make files.

; Provide a keybinding to run the nearest Makefile.
; TODO: Provide an interactive workflow for setting the target.
;       C-x C-a compiles without asking, and C-x C-n sets a new compile-command.

; Scroll the compilation buffer with new output.
(setq compilation-scroll-output t)

; Do not ask for the make command to run. This is set with the hook below.
(setq compilation-read-command nil)

; get-closest-pathname - to find the project make file.
; http://www.emacswiki.org/emacs/CompileCommand
(defun* get-closest-pathname (filename)
  "Determine the pathname of the first instance of FILE starting from the
  current directory towards root. This may not do the correct thing in presence
  of links. If it does not find FILE, then it shall return the name of FILE in
  the current directory, suitable for creation"
  (let ((root (expand-file-name "/"))) ; win32 builds should translate
    (expand-file-name filename
                      (loop
                       for d = default-directory then (expand-file-name ".." d)
                       if (file-exists-p (expand-file-name filename d))
                       return d
                       if (equal d root)
                       return nil))))

; Set compile-command to run on the nearest Makefile.
(defun use-nearest-makefile ()
  ; Compile using the nearest Makefile
  ; NOTE this only looks for Makefile, not makefile.
  (set (make-local-variable 'compile-command)
       ; Making "make -f %s" work across working directories is hard.
       ; Using "make -C %s" changes directories on make, works well.
       (format "make -C %s" (file-name-directory
                             (get-closest-pathname "Makefile")))))

(defun arduino-compile ()
  (interactive)
  (compile (format "make -f %s" (get-closest-pathname "Makefile"))))

(defun arduino-upload ()
  (interactive)
  (compile (format "make -f %s upload" (get-closest-pathname "Makefile"))))

; Set the compile-command for each buffer, in lieu of using compilation modes.
; Why? compilation modes have keymaps which override common key bindings.
(add-hook 'after-change-major-mode-hook
          '(lambda ()
             (let ((target (file-name-sans-extension (buffer-name)))
                   (extension (file-name-extension (buffer-name))))
               (cond
                ; Arduino
                ((string= "pde" extension)
                 (set (make-local-variable 'compile-command)
                      (format "make -f %s"
                              (get-closest-pathname "Makefile")))
                 (global-set-key (kbd "C-x C-a") 'arduino-compile)
                 (global-set-key (kbd "C-x C-u") 'arduino-upload))
                ; otherwise
                (t
                 (use-nearest-makefile))))))


;;; Modes - Programming Languages, Formats, & Frameworks
;;;
;;; Many language modes just work and are omitted here.

;; Arduino
; See more Arduino customizations in compile-command settings.
(add-to-list 'auto-mode-alist '("\\.pde$" . c++-mode))

;; C
(add-hook 'c-mode-hook 'flyspell-prog-mode)

;; C++
(add-hook 'c++-mode-hook 'flyspell-prog-mode)

;; CSS
(add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))

;; Common Lisp
(setq-default inferior-lisp-program "sbcl")

;; Conf
(add-hook 'conf-mode-hook 'flyspell-prog-mode)

;; Clojure
(add-hook 'clojure-mode-hook 'show-paren-mode)
(add-hook 'clojure-mode-hook 'flyspell-prog-mode)
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
; Skip :user section of ~/.lein/profiles.clj when using cider-jack-in.
(setq cider-lein-parameters
      "with-profile -user repl :headless :host localhost")

;; Cucumber
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\\.feature$" . feature-mode))

;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)

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
                             (setq indent-tabs-mode nil)
                             (setq standard-indent 2)
                             (setq tab-width 2)))

;; JavaScript
(require 'coffee-mode)
(add-hook 'js-mode-hook '(lambda ()
                           (auto-fill-mode -1)
                           (setq indent-tabs-mode nil)
                           (setq standard-indent 2)
                           (setq tab-width 2)))

;; Java
(add-hook 'java-mode-hook 'flyspell-prog-mode)

;; LaTeX
(add-to-list 'auto-mode-alist '("\\.latex$" . latex-mode))

;; Markdown
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; Python
(require 'python)
(add-hook 'python-mode-hook 'flyspell-prog-mode)

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
(add-hook 'ruby-mode-hook 'flyspell-prog-mode)
(add-hook 'ruby-mode-hook '(lambda ()
                             (setq standard-indent 2)))

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
               (setq fill-column 72)
               (setq goal-column nil)
               (setq comment-column 50))))

;; Apache Thrift
(require 'thrift)
(add-to-list 'auto-mode-alist '("\\.thrift$" . thrift-mode))

;; XML
(add-hook 'xml-mode-hook 'flyspell-prog-mode)

;; X
(add-to-list 'auto-mode-alist '("\\.xrdb$" . xrdb-mode))
