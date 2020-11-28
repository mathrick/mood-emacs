;; -*- lexical-binding: t; -*-

(when (memq (featurep! :system nil :os) '(:windows))
  ;; Windows compatibility
  (use-package ls-lisp
    :straight nil
    :config
    (setq! ls-lisp-dirs-first t)))
