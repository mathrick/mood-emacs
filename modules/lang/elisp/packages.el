;; -*- lexical-binding: t; -*-

(unless (featurep! -macrostep)
  (use-package macrostep))

(when (featurep! +nameless)
  (use-package nameless
    :hook (emacs-lisp-mode . nameless-mode)
    :config (setq! nameless-private-prefix t)))

(unless (featurep! -rainbow)
  (use-package rainbow-delimiters
    :hook ((lisp-mode . rainbow-delimiters-mode)
           (emacs-lisp-mode . rainbow-delimiters-mode))))
