;; -*- lexical-binding: t; -*-

(unless (feature! -macrostep)
  (use-package macrostep))

(when (feature! +nameless)
  (use-package nameless
    :hook (emacs-lisp-mode . nameless-mode)
    :config (setq! nameless-private-prefix t)))

(unless (feature! -rainbow)
  (use-package rainbow-delimiters
    :hook ((lisp-mode . rainbow-delimiters-mode)
           (emacs-lisp-mode . rainbow-delimiters-mode))))

(unless (feature! -edebug-x)
  (use-package edebug-x))
