;; -*- lexical-binding: t; -*-

(unless (featurep! -anaconda)
  (when (member (featurep! :server) '(nil t :anaconda))
    (use-package anaconda-mode
      :hook (python-mode
             (python-mode . anaconda-eldoc-mode)))
    ;; FIXME: this will install company-anaconda even if the user
    ;; didn't select company. We need glue layer handling
    (use-package company-anaconda
      :after (company anaconda-mode)
      :config (add-to-list 'company-backends 'company-anaconda))))

(when (featurep! +pyenv)
  (use-package pyenv-mode
    :hook python-mode)
  (use-package pyenv-mode-auto
    :after pyenv-mode))
