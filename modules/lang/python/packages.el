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
          :config (add-to-list 'company-backends
                               '(company-anaconda :with company-capf)))))))

(unless (featurep! -pyenv)
  (use-package pyenv-mode
    :hook python-mode)
  (use-package pyenv-mode-auto
    :after pyenv-mode))

(unless (featurep! -pyvenv)
  (use-package pyvenv))

(unless (or (featurep! -pyenv)
            (featurep! -pyvenv))
  (use-package pyvenv
    :after pyenv-mode
    ;; Disable pyenv-mode if pyvenv is active and vice-versa, since
    ;; they both deal with virtual envs
    :config
    ;; can't use :hook because pyvenv misnames its hook variables
    ;; (they end in -hooks, not -hook)
    (add-hook 'pyvenv-post-activate-hooks (lambda () (pyenv-mode -1)))
    (add-hook 'pyvenv-post-deactivate-hooks (lambda () (pyenv-mode 1)))))

(unless (featurep! -pyvenv-restart)
  (use-package pyvenv
    :config
    (add-hook 'pyvenv-post-activate-hooks 'pyvenv-restart-python)))


