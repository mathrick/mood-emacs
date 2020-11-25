;; -*- lexical-binding: t; -*-

(unless (featurep! -file-history)
  ;; Remember the last position in previously visited files
  (use-package saveplace
    :config (save-place-mode t))

  (use-package recentf
    :config (recentf-mode)))
