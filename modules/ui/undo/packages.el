;; -*- lexical-binding: t; -*-

(if (feature! +fu)
    (progn
      (use-package undo-fu
        :general ("C-_" undo-fu-only-undo)
                 ("M-_" undo-fu-only-redo))
      (when (feature! +session)
        (use-package undo-fu-session
          :after undo-fu
          :config (global-undo-fu-session-mode))))
  (use-package undo-tree
    :config
    ;; Undo conflicting bindings
    (when (feature! :editing expand-region :enabled)
      (general-unbind undo-tree-map "C-?"))
    (when (feature! :editing smartparens :enabled)
      (general-unbind undo-tree-map "M-?"))
    (global-undo-tree-mode)))
