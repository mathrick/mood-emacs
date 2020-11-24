;; -*- lexical-binding: t; -*-

(use-package company
  :hook (after-init . global-company-mode))

(unless (or (featurep! -box)
            (not (featurep! :system nil :gui)))
  (use-package company-box
    :after company
    :hook (company-mode . company-box-mode)))
