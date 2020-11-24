;; -*- lexical-binding: t; -*-

(unless (featurep! -anaconda)
  (when (member (featurep! :server) '(nil t :anaconda))
    (let ((company-enabled (featurep! :editing company :enabled)))
      (use-package anaconda-mode
        :hook (python-mode
               (python-mode . anaconda-eldoc-mode)))
      ;; FIXME: It would be nice if that could be done declaratively
      ;; through a glue layer, without anaconda needing to know about
      ;; company
      (when company-enabled
        (use-package company-anaconda
          :after (company anaconda-mode)
          :config (add-to-list 'company-backends 'company-anaconda))))))

(when (featurep! +pyenv)
  (use-package pyenv-mode
    :hook python-mode)
  (use-package pyenv-mode-auto
    :after pyenv-mode))
