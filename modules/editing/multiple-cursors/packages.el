;; -*- lexical-binding: t; -*-

(use-package multiple-cursors
  :demand t
  :bind
  (("C-c C->" . mc/edit-lines)
   ("C->" . mc/mark-more-like-this-extended)
   ("C-<" . mc/mark-previous-like-this)
   ("C-M->" . mc/skip-to-next-like-this)
   ("M->" . mc/mark-all-dwim)))

(unless (feature! +cancel-on-ret)
  (use-package multiple-cursors
    :bind
    (:map mc/keymap
          ;; I hate RET cancelling MC, I have C-g for that
          ("<return>" . nil))))

(use-package mc-cycle-cursors
  ;; included in multiple-cursors
  :straight nil
  :after multiple-cursors)

(use-package rectangular-region-mode
  :straight nil
  :after multiple-cursors
  :bind ("M-<return>" . set-rectangular-region-anchor))
