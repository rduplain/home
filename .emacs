(setq inhibit-startup-message t)
(setq-default require-final-newline t)
(setq default-major-mode 'text-mode)
;(add-hook 'text-mode-hook '(lambda () (auto-fill-mode 1)))
;(setq-default line-number-mode t)
(global-set-key "\C-x\C-b" 'electric-buffer-list)
(menu-bar-mode nil)
(setq standard-indent 4)
(setq-default indent-tabs-mode nil)
(mouse-wheel-mode t)
(setq-default fill-column 80)
;(setq-default auto-fill-function 'do-auto-fill)
(setq-default column-number-mode 1)
(global-font-lock-mode 1)
(setq initial-scratch-message nil)

;(require 'muse-mode)

;(require 'muse-html)
;(require 'muse-latex)
;(require 'muse-texinfo)
;(require 'muse-docbook)

;(require 'muse-project)


(defun insert-date (&optional insert-date-format)
  "Insert the current date formatted to the given argument."
  (interactive)
  (unless insert-date-format (setq insert-date-format "%a %b %d %T %Z %Y\n"))
  (insert (format-time-string insert-date-format)))
(global-set-key (kbd "<f5>") 'insert-date)

(global-unset-key (kbd "C-t"))
;(defvar thoughtview-map nil)
;(setq thoughtview-map (make-sparse-keymap))

;(global-set-key "\C-t" thoughtview-map)

(defun insert-modified-tag ()
  "Insert the current time in modified tag format."
  (interactive)
  (insert-date "+modified:::%Y%m%d %T\n"))
(global-set-key (kbd "C-t m") 'insert-modified-tag)

(setq load-path (cons "~/.emacs.d" load-path))

(autoload 'ruby-mode "ruby-mode" "Major mode for editing ruby files." t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml$" . html-mode))

(add-to-list 'auto-mode-alist '("\\.R$" . R-mode))
(add-to-list 'auto-mode-alist '("\\.r$" . R-mode))

(add-hook 'ruby-mode-hook
          (lambda()
            (add-hook 'local-write-file-hooks
                      '(lambda()
                         (save-excursion
                           (untabify (point-min) (point-max))
                           (delete-trailing-whitespace)
                           )))
            (set (make-local-variable 'indent-tabs-mode) 'nil)
            (set (make-local-variable 'tab-width) 2)
            (imenu-add-to-menubar "IMENU")
            (require 'ruby-electric)
            (ruby-electric-mode t)
            ))

(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(global-set-key "\C-cc" 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key "\C-ck" 'mode-compile-kill)
