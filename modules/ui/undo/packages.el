;; -*- lexical-binding: t; -*-

(if (featurep! +fu)
    (progn
      (use-package undo-fu
        :general ("C-_" undo-fu-only-undo)
                 ("M-_" undo-fu-only-redo))
      (when (featurep! +session)
        (use-package undo-fu-session
          :after undo-fu
          :config (global-undo-fu-session-mode))))
  (use-package undo-tree
    :config (global-undo-tree-mode)))
