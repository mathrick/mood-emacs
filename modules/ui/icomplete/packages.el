;; -*- lexical-binding: t; -*-

(use-package icomplete
  :config (icomplete-mode))

(when (featurep! +vertical)
  (use-package icomplete-vertical
    :demand t
    :config
    (icomplete-vertical-mode)
    :bind (:map icomplete-minibuffer-map
                ("<down>" . icomplete-forward-completions)
                ("C-n" . icomplete-forward-completions)
                ("<up>" . icomplete-backward-completions)
                ("C-p" . icomplete-backward-completions)
                ("C-v" . icomplete-vertical-toggle))))
