;; -*- lexical-binding: t; -*-


(let ((-strict (featurep! -strict)))
  (use-package smartparens
    :config (if -strict
                (smartparens-global-mode)
              (smartparens-global-strict-mode))))
