;; -*- lexical-binding: t; -*-

(unless (featurep! -parens)
  ;;; Parens and cursor
  (use-package paren
    :config
    (show-paren-mode)
    (setq blink-matching-paren nil))

  (use-package highlight-parentheses
    :hook (prog-mode . global-highlight-parentheses-mode)))

;; CamelCase is a bit tedious to read, this helps with that
(unless (featurep! -glasses)
  (use-package glasses
    :config
    (setq! glasses-face 'bold)
    (setq! glasses-original-separator "")
    (setq! glasses-separator "")
    (setq! glasses-separate-parentheses-p nil)
    :hook (prog-mode . glasses-mode)))

(unless (featurep! -comment-dwim)
  (use-package comment-dwim-2
    :general ("M-;" #'comment-dwim-2))
  (when (featurep! :editing org :enabled)
    (use-package comment-dwim-2
    :general (org-mode-map "M-;" #'org-comment-dwim-2))))
