;;;; Personal theme for company-mode, simple colors for Emacs inside terminal.

(let* ((background "black")
       (foreground "white")
       (primary "blue")
       (secondary "cyan")

       (bg background)
       (fg foreground)
       (annotation-bg background)
       (annotation-fg primary)
       (field-bg background)
       (field-fg secondary)
       (mouse-bg primary)
       (mouse-fg background)
       (preview-search-bg primary)
       (preview-search-fg foreground)
       (search-bg secondary)
       (search-fg background)
       (select-bg primary)
       (select-fg foreground)
       (ann-select-bg select-bg)
       (ann-select-fg secondary)
       (scroll-bg background)
       (scroll-fg background))

  (custom-set-faces
   `(company-echo-common
     ((t (:inherit company-echo))))
   `(company-preview
     ((t (:foreground ,fg :underline t))))
   `(company-preview-common
     ((t (:inherit company-preview))))
   `(company-preview-search
     ((t (:background ,preview-search-bg :foreground ,preview-search-fg))))
   `(company-scrollbar-bg
     ((t (:background ,scroll-bg :foreground ,scroll-bg))))
   `(company-scrollbar-fg
     ((t (:background ,scroll-fg :foreground ,scroll-fg))))
   `(company-template-field
     ((t (:background ,field-bg :foreground ,field-fg))))
   `(company-tooltip
     ((t (:background ,bg :foreground ,fg))))
   `(company-tooltip-annotation
     ((t (:background ,annotation-bg :foreground ,annotation-fg))))
   `(company-tooltip-annotation-selection
     ((t (:background ,ann-select-bg :foreground ,ann-select-fg))))
   `(company-tooltip-common
     ((((type x)) (:inherit company-tooltip :weight bold))
      (t (:inherit company-tooltip))))
   `(company-tooltip-common-selection
     ((((type x)) (:inherit company-tooltip-selection :weight bold))
      (t (:inherit company-tooltip-selection))))
   `(company-tooltip-mouse
     ((t (:background ,mouse-bg :foreground ,mouse-fg))))
   `(company-tooltip-search
     ((t (:background ,search-bg :foreground ,search-fg))))
   `(company-tooltip-search-selection
     ((t (:inherit company-tooltip-search))))
   `(company-tooltip-selection
     ((t (:background ,select-bg :foreground ,select-fg :weight bold))))))
