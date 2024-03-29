;; -*- lexical-binding: t; -*-

(unless (feature! +indent-tabs)
  ;; Tabs to indent are of the devil
  (setq-default indent-tabs-mode nil))

(unless (feature! -parens)
  ;;; Parens and cursor
  (use-package paren
    :config
    (show-paren-mode)
    (setq blink-matching-paren nil))

  (use-package highlight-parentheses
    :hook (prog-mode . global-highlight-parentheses-mode)))

;; CamelCase is a bit tedious to read, this helps with that
(unless (feature! -glasses)
  (use-package glasses
    :config
    (setq! glasses-face 'bold)
    (setq! glasses-original-separator "")
    (setq! glasses-separator "")
    (setq! glasses-separate-parentheses-p nil)
    :hook (prog-mode . glasses-mode)))

(unless (feature! -comment-dwim)
  (let ((style (feature! :comment-dwim)))
    (use-package comment-dwim-2
      :config
      ;; By default, CD2 extends partial lines, rather than commenting
      ;; out the region as marked, as `comment-dwim' does. This
      ;; reverts to the way `comment-dwim' works, which is useful in
      ;; its own right
      (unless (eq style 'partial-lines)
	(setq! cd2/region-command 'cd2/comment-or-uncomment-region))
     :general ("M-;" #'comment-dwim-2)))

  (when (feature! :editing org :enabled)
    (use-package comment-dwim-2
      :general (org-mode-map "M-;" #'org-comment-dwim-2))))
