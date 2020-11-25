;; -*- lexical-binding: t; -*-

(use-package icomplete
  :config (icomplete-mode))

(when (featurep! +vertical)
  (use-package icomplete-vertical
    :config (icomplete-vertical-mode)))
