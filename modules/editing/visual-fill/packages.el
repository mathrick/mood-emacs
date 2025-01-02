;; -*- lexical-binding: t; -*-

(use-package visual-fill-column
  ;;; Limit visual-line-mode to fill-column, instead of window width
  :hook (visual-line-mode . (lambda ()
                              ;; Just visual-fill-column-mode will not turn it off properly,
                              ;; see https://codeberg.org/joostkremers/visual-fill-column/issues/5
                              (visual-fill-column-mode (if visual-line-mode 1 -1)))))

(unless (feature! -adaptive-wrap)
  ;;; Nicer wrapping in lists and such when in visual-line-mode
  (use-package adaptive-wrap
    :hook (visual-line-mode . adaptive-wrap-prefix-mode)))
