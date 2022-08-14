;; -*- lexical-binding: t; -*-

(use-package visual-fill-column
  ;;; Limit visual-line-mode to fill-column, instead of window width
  :hook (visual-line-mode . visual-fill-column-mode))

(unless (featurep! -adaptive-wrap)
  ;;; Nicer wrapping in lists and such when in visual-line-mode
  (use-package adaptive-wrap
    :hook (visual-line-mode . adaptive-wrap-prefix-mode)))
