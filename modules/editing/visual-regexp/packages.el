;; -*- lexical-binding: t; -*-

(use-package visual-regexp
  :general ("C-%" 'vr/replace
            "M-%" 'vr/query-replace

            "C-x R R" 'vr/replace
            "C-x R Q" 'vr/query-replace
            "C-x R C" 'vr/mc-mark))

(unless (featurep! -steroids)
  (use-package pcre2el :defer t)

  (use-package visual-regexp-steroids
    :config
    (setq! vr/engine 'pcre2el)
    ;; Let the compiler know it's a special var
    (defvar vr/engine)
    ;; Wrapper advice which makes isearch use visual-regexp-steroids
    (defun vr/wrap-isearch-fun (fun &rest args)
      ;; Must be interactive, otherwise APPLY will invoke isearch in non-modal
      ;; mode, ie. it will return immediately and thus undo our change to the
      ;; search func.
      (interactive)
      (let* ((isearch-search-fun-function (if (eq vr/engine 'emacs)
                                              isearch-search-fun-function
                                            #'vr--isearch-search-fun-function)))
        (apply fun args)))

    (advice-add #'isearch-backward        :around #'vr/wrap-isearch-fun)
    (advice-add #'isearch-forward         :around #'vr/wrap-isearch-fun)
    (advice-add #'isearch-backward-regexp :around #'vr/wrap-isearch-fun)
    (advice-add #'isearch-forward-regexp  :around #'vr/wrap-isearch-fun)))
