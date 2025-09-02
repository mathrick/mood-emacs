;; -*- lexical-binding: t; -*-

(use-package p4
  :straight (:repo "JohnC32/perforce-emacs" :branch "main"))

(unless (or (feature! -magit)
            (not (feature! :vcs/magit/enabled)))
  (use-package magit-p4
    :after magit))
