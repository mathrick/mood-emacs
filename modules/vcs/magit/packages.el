;; -*- lexical-binding: t; -*-

(use-package magit
  :defer t)

(unless (featurep! -forge)
  (use-package forge
    :after magit))
