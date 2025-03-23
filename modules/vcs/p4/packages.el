;; -*- lexical-binding: t; -*-

(use-package p4
  :straight (:fork (:repo "JohnC32/perforce-emacs")))

(unless (or (feature! -magit)
            (not (feature! :vcs/magit/enabled)))
  (use-package magit-p4
    :after magit))
