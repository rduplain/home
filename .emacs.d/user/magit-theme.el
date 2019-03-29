;;;; Personal theme for magit.

;;; `magit-diffstat-added' provides a simple face for added lines.
;;; `magit-diffstat-removed' provides a simple face for removed lines.

(custom-set-faces
 `(magit-diff-added
   ((t (:inherit magit-diffstat-added))))
 `(magit-diff-added-highlight
   ((t (:inherit magit-diff-added))))
 `(magit-diff-base
   ((t (:inherit magit-diffstat-removed))))
 `(magit-diff-base-highlight
   ((t (:inherit magit-diff-base))))
 `(magit-diff-lines-boundary
   ((t (:inherit magit-diffstat-removed))))
 `(magit-diff-lines-heading
   ((t (:inherit magit-diffstat-removed))))
 `(magit-diff-our
   ((t (:inherit magit-diffstat-removed))))
 `(magit-diff-our-highlight
   ((t (:inherit magit-diff-our))))
 `(magit-diff-removed
   ((t (:inherit magit-diffstat-removed))))
 `(magit-diff-removed-highlight
   ((t (:inherit magit-diff-removed))))
 `(magit-diff-their
   ((t (:inherit magit-diffstat-added))))
 `(magit-diff-their-highlight
   ((t (:inherit magit-diff-their)))))
