;;; Emacs configuration for Ron DuPlain.
;;; Based on those of Jeff Uphoff, Eric Sessoms, and Internet posters.
;;; GNU Emacs 23


;;; Basics

(require 'cl)

;; Define the load path.
(setq load-path (cons "~/.emacs.d" load-path))
(setq load-path (cons "~/.emacs.d/bookmark+" load-path))
(setq load-path (cons "~/.emacs.d/scala-mode" load-path))
(setq load-path (cons "~/.emacs.d/magit" load-path))
(setq load-path (cons "~/.emacs.d/yasnippet" load-path))

;; Use Emacs Lisp Package Archive package manager.
(when (load (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

;; Disable useless decorations.
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Disable the menu bar.
(menu-bar-mode nil)

;; Require a final newline (and prompt if not exists).
(setq-default require-final-newline t)

;; Accept y or n when presented with yes or no.
(fset 'yes-or-no-p 'y-or-n-p)

;; Disable beeps.
(setq ring-bell-function 'ignore)

;; Disable tramp.
(setq tramp-mode nil)

;; Always use syntax highlighting.
(global-font-lock-mode 1)

;; If you are using a dark background, set to dark.  Comment out otherwise.
;; This is essential for rst-mode, where sections & blocks are highlighted.
;; See rst.el for more details.
(setq frame-background-mode 'dark)

;; Show the column number in addition to the line number.
(setq-default column-number-mode 1)

;; Don't just show me buffers, interact!
(global-set-key "\C-x\C-b" 'electric-buffer-list)

;; Indent only with spaces (default 4), never tabs.
(setq-default standard-indent 4)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Fill columns at 80 characters.
(setq-default fill-column 79)

;; Truncate long lines on partial windows.
(setq-default truncate-partial-width-windows t)

;; Remove trailing whitespace before saving files.
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;;; Extensions

;; Use magit for git interactions.
(require 'magit)
(global-set-key "\C-xg" 'magit-status)

;; Use bookmark+.
(require 'bookmark+)

;; Extend dired.
(eval-after-load "dired"
  '(require 'dired-x))

; Ignore uninteresting files.
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-omit-mode 1)))

; Add more uninteresting files.
; Archive files are uninteresting in emacs.
(setq dired-omit-files
      (concat dired-omit-files "\\|\\.zip$"))

(setq dired-omit-files
      (concat dired-omit-files "\\|\\.tar\\.gz$\\|\\.tgz$"))

(setq dired-omit-files
      (concat dired-omit-files "\\|\\.tar\\.bz$\\|\\.tar\\.bz2$\\|\\.tbz$\\|\\.tbz2$"))

; Swap files from vi/vim are uninteresting.
(setq dired-omit-files
      (concat dired-omit-files "\\|\\.sw[op]$"))

; Build output directories are uninteresting.
(setq dired-omit-files
      (concat dired-omit-files "\\|^dist$"))

; Data and database files are uninteresting in emacs.
(setq dired-omit-files
      (concat dired-omit-files "\\|\\.dat$\\|\\.sqlite3?$"))

(setq dired-omit-files
      (concat dired-omit-files "\\|\\.db$\\|\\.db-journal$"))

; Python "egg" files are uninteresting.
(setq dired-omit-files
      (concat dired-omit-files "\\|\\.egg$\\|\\.egg-info$"))

; Python test tool files are uninteresting.
(setq dired-omit-files
      (concat dired-omit-files "\\|\\.coverage$\\|\\.tox$"))

; Mac OS X clutter is uninteresting.
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\.DS_Store$\\|^__MACOSX$"))

; Log files are interesting, but not all the time.
(setq dired-omit-files
      (concat dired-omit-files "\\|\\.log$"))


;;; Fixes

;; Some terminals (incl. PuTTY) send [select] on home keypress.
(define-key global-map [select] 'end-of-line)

;;; Suspend vs. Shell

;; Disable suspend.
(global-unset-key (kbd "C-z"))

;; Bind to shell instead of suspending.
(global-set-key (kbd "C-z") 'ansi-term)


;;; Convenient Functions

;; Insert the date with F5, formatted just like the UNIX date command.
(defun insert-date (&optional insert-date-format)
  "Insert the current date formatted to the given argument."
  (interactive)
  (unless insert-date-format (setq insert-date-format "%a %b %d %T %Z %Y\n"))
  (insert (format-time-string insert-date-format)))

(global-set-key [f5] 'insert-date)

;; Enlarge/shrink windows with ease with Control-6 and Meta-6.
(global-set-key [?\C-^] 'enlarge-window)
;TODO: This breaks repeat-by-number prefixing. Fix it.
;(global-set-key [?\M-6] 'shrink-window)

;; yic buffer cycle - http://www.dotemacs.de/dotfiles/BenjaminRutt.emacs.html
(defun yic-ignore (str)
  (or
   ;;buffers I don't want to switch to
   (string-match "^\\*scratch\\*$" str)
   (string-match "\\*Buffer List\\*" str)
   (string-match "^TAGS" str)
   (string-match "^\\*Messages\\*$" str)
   (string-match "^\\*Completions\\*$" str)
   (string-match "^\\*ESS\\*$" str)
   (string-match "^\\*Pymacs\\*$" str)
   (string-match "^ " str)

   ;;Test to see if the window is visible on an existing visible frame.
   ;;Because I can always ALT-TAB to that visible frame, I never want to
   ;;Ctrl-TAB to that buffer in the current frame.  That would cause
   ;;a duplicate top-level buffer inside two frames.
   (memq str
         (mapcar
          (lambda (x)
            (buffer-name
             (window-buffer
              (frame-selected-window x))))
          (visible-frame-list)))))

(defun yic-next (ls)
  "Switch to next buffer in ls skipping unwanted ones."
  (let* ((ptr ls) bf bn go)
    (while (and ptr (null go))
      (setq bf (car ptr)  bn (buffer-name bf))
      (if (null (yic-ignore bn)) ;skip over
          (setq go bf)
        (setq ptr (cdr ptr))))
    (if go (switch-to-buffer go))))

(defun yic-prev-buffer ()
  "Switch to previous buffer in current window."
  (interactive)
  (yic-next (reverse (buffer-list))))

(defun yic-next-buffer ()
  "Switch to the other buffer (2nd in list-buffer) in current window."
  (interactive)
  (bury-buffer (current-buffer))
  (yic-next (buffer-list)))

(global-set-key (kbd "<M-RET>") 'yic-next-buffer)

;; dos2unix/unix2dos - http://www.dotemacs.de/dotfiles/BenjaminRutt.emacs.html
(defun dos2unix ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defun unix2dos ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))


;;; Modes - General Purpose

;; Set the default mode to Text.
(setq default-major-mode 'text-mode)

;; Interactively Do Things, with fuzzy matching enabled.
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; Redo
(require 'redo)
(global-set-key (kbd "C-/") 'undo)
(global-set-key (kbd "C-\\") 'redo)

;; Winner, undo and redo window configuration changes.
(winner-mode t)
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

; Override default key-binding.
(setq flyspell-use-meta-tab nil)
(global-unset-key [?\M-s])
(setq flyspell-auto-correct-binding [?\M-s])

;; Yet Another Snippet system, for code/text snippets.
(require 'yasnippet)
(setq yas-global-mode 1)
(setq yas-snippet-dirs '("~/.emacs.d/snippets" ; highest priority
                         "~/.emacs.d/yasnippet/snippets"))

;; Drag stuff.
(require 'drag-stuff)
(drag-stuff-global-mode t)
(defvar drag-stuff-mode-map (make-sparse-keymap)
  "Keymap for `drag-stuff-mode'.")
(define-key drag-stuff-mode-map [(control return)] 'drag-stuff-down)
;(define-key drag-stuff-mode-map [(control shift 'P)] 'drag-stuff-up)
;(define-key drag-stuff-mode-map (kbd "C-N") 'drag-stuff-down)
;(define-key drag-stuff-mode-map (kbd "S-C-f") 'drag-stuff-right)
;(define-key drag-stuff-mode-map (kbd "S-C-b") 'drag-stuff-left)

;;; Programming Tools

;; Compilation: compile/recompile - great for make files.

; Provide a keybinding to run the nearest Makefile.
(global-set-key (kbd "C-x C-a") 'compile)
; TODO: Consider special make keybindings.
;(global-set-key (kbd "C-x C-n") 'compile) ; make clean
;(global-set-key (kbd "C-x C-j") 'compile) ; make flakes
; or... C-x C-a compiles without asking, and C-x C-n sets a new compile-command.

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

;; Arduino
; See more Arduino customizations in compile-command settings.
(add-to-list 'auto-mode-alist '("\\.pde$" . c++-mode))

;; C
(add-hook 'c-mode-hook 'flyspell-prog-mode)

;; C++
(add-hook 'c++-mode-hook 'flyspell-prog-mode)

;; Cappuccino
(add-to-list 'auto-mode-alist '("\\.j$" . objc-mode))
(add-hook 'objc-mode-hook 'flyspell-prog-mode)

;; Common Lisp
(setq inferior-lisp-program "sbcl") ; default, override with hooks
(load (expand-file-name "~/.quicklisp/slime-helper.el"))

;; Conf
(add-hook 'conf-mode-hook 'flyspell-prog-mode)

;; Cucumber/Lettuce
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\\.feature$" . feature-mode))

;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)

;; Haskell
(load "~/.emacs.d/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(add-hook 'haskell-mode-hook 'flyspell-prog-mode)

;; HTML
(add-to-list 'auto-mode-alist '("\\.html$" . html-mode))
(add-hook 'html-mode-hook 'flyspell-prog-mode)
(add-hook 'html-mode-hook '(lambda ()
                             (auto-fill-mode nil)
                             (setq indent-tabs-mode nil)
                             (setq standard-indent 2)
                             (setq tab-width 2)
                             (setq sgml-basic-offset 2)))

;; JavaScript
(add-hook 'js-mode-hook '(lambda ()
                           (setq auto-fill-mode nil)
                           (setq indent-tabs-mode nil)
                           (setq standard-indent 2)
                           (setq tab-width 2)))

;; Java
(add-hook 'java-mode-hook 'flyspell-prog-mode)

;; Python
; Drop this python.el in .emacs.d:
; https://github.com/fgallina/python.el/tree/emacs23
(require 'python)
(add-hook 'python-mode-hook 'flyspell-prog-mode)
(add-hook 'python-mode-hook '(lambda () (require 'virtualenv)))

(if (not (boundp 'python-python-command))
    (setq python-python-command "python"))
(if (not (boundp 'python-python-command-args))
    (setq python-python-command-args ""))

(defun ropemacs-init ()
  "Setup ropemacs, on Pymacs."
  (autoload 'pymacs-apply "pymacs")
  (autoload 'pymacs-call "pymacs")
  (autoload 'pymacs-eval "pymacs" nil t)
  (autoload 'pymacs-exec "pymacs" nil t)
  (autoload 'pymacs-load "pymacs" nil t)
  (setq ropemacs-global-prefix "C-x 7") ; unused
  (pymacs-load "ropemacs" "rope-")
  (setq ropemacs-enable-autoimport t)
  (setq ropemacs-guess-project t)
  (setq ropemacs-codeassist-maxfixes 3))

(defun ropemacs-init-if-needed ()
  "Setup ropemacs if it has not already been initialized."
  (if (not (fboundp 'rope-code-assist))
      (progn (ropemacs-init)
             ; Since ropemacs was not loaded, we must set the mode now.
             (ropemacs-mode))))

; Lazily load Pymacs & ropemacs, loading only on first instance of a buffer
; with python-mode. This prevents unnecessary Python subprocesses on emacs
; instances without python-mode.
(add-hook 'python-mode-hook 'ropemacs-init-if-needed)

; Python tab behavior:
;
; 1. expand a snippet via yasnippet if a match is found, or
; 2. indent if point is at beginning of line, or
; 3. offer code completions in mini-buffer (with ido mode) with ropemacs

; In yasnippet 0.8.0, the default key is <tab>, and the default fallback
; behavior is to call the command bound to tab if yasnippet is unable to expand
; anything.
(define-key python-mode-map "\t" 'indent-or-code-assist)

(defun point-at-line-beginning ()
  (eql 0 (string-match "\\W*$" (buffer-substring (line-beginning-position) (point)))))

(defun indent-or-code-assist ()
  "Indent if point at beginning of line, else offer code assistance via rope."
  (interactive)
  (if (point-at-line-beginning)
      (indent-for-tab-command)
    (if (rope-completions)
        (call-interactively 'rope-code-assist))))

;; R
(add-to-list 'auto-mode-alist '("\\.R$" . R-mode))
(add-to-list 'auto-mode-alist '("\\.r$" . R-mode))
(add-hook 'ess-mode-hook 'flyspell-prog-mode)

;; ReStructuredText (rst)
(add-hook 'rst-mode-hook '(lambda () (auto-fill-mode nil)))

;; Ruby
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml$" . html-mode))
(add-hook 'ruby-mode-hook 'flyspell-prog-mode)
(add-hook 'ruby-mode-hook '(lambda () (setq standard-indent 2)))

;; Scala
(require 'scala-mode-auto)

;; Shell
(add-hook 'sh-mode-hook 'flyspell-prog-mode)

;; Text
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook
          '(lambda ()
             ; Declare defaults here.
             (auto-fill-mode 1)
             ; Handle git commit message editing.
             (if (string= "COMMIT_EDITMSG" (buffer-name))
                 (progn
                   (ruler-mode 1)
                   (setq fill-column 72)
                   (setq goal-column nil)
                   (setq comment-column 50)))))

;; XML
(add-hook 'xml-mode-hook 'flyspell-prog-mode)


;;; Modes to Consider

;; Abbrev
;; Desktop
;; Org
;; Paredit


;;; Auto-generated customizations.
(custom-set-variables
 '(bmkp-last-as-first-bookmark-file "~/.emacs.bmk"))
