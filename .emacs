;;; Emacs configuration for Ron DuPlain.
;;; Based on those of Jeff Uphoff, Eric Sessoms, and Internet posters.
;;; GNU Emacs 23


;;; Basics

;; Define the load path.
(setq load-path (cons "~/.emacs.d" load-path))

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

;; Always use syntax highlighting.
(global-font-lock-mode 1)

;; Show the column number in addition to the line number.
(setq-default column-number-mode 1)

;; Don't just show me buffers, interact!
(global-set-key "\C-x\C-b" 'electric-buffer-list)

;; Indent only with spaces (default 4), never tabs.
(setq standard-indent 4)
(setq-default indent-tabs-mode nil)

;; Fill columns at 80 characters.  Auto-fill in Text mode.
(setq-default fill-column 80)
(add-hook 'text-mode-hook 'turn-on-auto-fill)


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

; Experimenting with a personal C-t keyboard map.
(global-unset-key (kbd "C-t"))

;; Insert the date with a modified tag (experimental structured txt).
(defun insert-modified-tag ()
  "Insert the current time in modified tag format."
  (interactive)
  (insert-date "+modified:::%Y%m%d %T\n"))
(global-set-key (kbd "C-t m") 'insert-modified-tag)

;; yic buffer cycle - http://www.dotemacs.de/dotfiles/BenjaminRutt.emacs.html
(defun yic-ignore (str)
  (or
   ;;buffers I don't want to switch to
   (string-match "\\*Buffer List\\*" str)
   (string-match "^TAGS" str)
   (string-match "^\\*Messages\\*$" str)
   (string-match "^\\*Completions\\*$" str)
   (string-match "^\\*ESS\\*$" str)
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
          (visible-frame-list)))
   ))

(defun yic-next (ls)
  "Switch to next buffer in ls skipping unwanted ones."
  (let* ((ptr ls) bf bn go)
    (while (and ptr (null go))
      (setq bf (car ptr)  bn (buffer-name bf))
      (if (null (yic-ignore bn)) ;skip over
          (setq go bf)
        (setq ptr (cdr ptr))
        )
      )
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


;;; Modes - Programming Languages, Formats, & Frameworks

;; C
(add-hook 'c-mode-hook 'flyspell-prog-mode)

;; Cappuccino
(add-to-list 'auto-mode-alist '("\\.j$" . objc-mode))
(add-hook 'objc-mode-hook 'flyspell-prog-mode)

;; Conf
(add-hook 'conf-mode-hook 'flyspell-prog-mode)

;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)

;; Erlang
(add-to-list 'auto-mode-alist '("\\.et$" . html-mode))
(require 'erlang-start)

(defun get-erl-man ()
  (interactive)
  (let* ((man-path "/usr/lib/erlang/man")
         (man-args (format "-M %s %s" man-path (current-word))))
    (man man-args)))

(add-hook 'erlang-mode-hook
          (lambda ()
            (local-set-key [(f6)] (lambda () (interactive) (get-erl-man)))
            ))
(add-hook 'erlang-mode-hook 'flyspell-prog-mode)

;; Haskell
(load "~/.emacs.d/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(add-hook 'haskell-mode-hook 'flyspell-prog-mode)

;; HTML
(add-hook 'html-mode-hook 'flyspell-prog-mode)

;; Java
(add-hook 'java-mode-hook 'flyspell-prog-mode)

;; Python
(add-hook 'python-mode-hook 'flyspell-prog-mode)
(add-hook 'python-mode-hook '(lambda () (require 'virtualenv)))

;; R
(add-to-list 'auto-mode-alist '("\\.R$" . R-mode))
(add-to-list 'auto-mode-alist '("\\.r$" . R-mode))
(add-hook 'ess-mode-hook 'flyspell-prog-mode)

;; Ruby
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml$" . html-mode))
(add-hook 'ruby-mode-hook 'flyspell-prog-mode)

;; Shell
(add-hook 'sh-mode-hook 'flyspell-mode)

;; Text
(add-hook 'text-mode-hook 'flyspell-mode)

;; XML
(add-hook 'xml-mode-hook 'flyspell-prog-mode)


;;; Modes to Consider

;; Abbrev
;; Compile
;; Desktop (!)
;; Org
;; Paredit (lisp)
;; YASnippet (or another snippet system)


;;; Available Keys

;; [f7]
;; [?\C-^]
