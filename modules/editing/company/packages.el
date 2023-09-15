;; -*- lexical-binding: t; -*-

(use-package company
  :hook (after-init . global-company-mode))

(unless (or (feature! -box)
            (not (feature! :system nil :gui)))
  (use-package company-box
    :hook (company-mode . company-box-mode)))
