;; -*- lexical-binding: t; -*-

(when (memq (feature! :system nil :os) '(:windows))
  ;; Windows compatibility
  (use-package ls-lisp
    :straight nil
    :config
    (setq! ls-lisp-dirs-first t)))
