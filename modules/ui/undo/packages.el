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
    :config
    ;; Undo conflicting bindings
    (when (featurep! :editing expand-region :enabled)
      (general-unbind undo-tree-map "C-?"))
    (when (featurep! :editing smartparens :enabled)
      (general-unbind undo-tree-map "M-?"))
    (global-undo-tree-mode)))
