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
    ;; Wrapper which makes isearch use visual-regexp-steroids
    (defun vr/wrap-isearch-fun (fun &optional regexp)
      (let* ((isearch-search-fun-function (if (eq vr/engine 'emacs)
                                              isearch-search-fun-function
                                            #'vr--isearch-search-fun-function)))
        ;; NOTE: We specifically take regexp arg here instead of using
        ;; `call-interactively', since doing so will invoke isearch in
        ;; non-modal mode, ie. it will return immediately and thus
        ;; undo our change to the search func.
        (funcall fun regexp)))

    (defun vr/isearch-backward (&optional regexp)
      (interactive "P")
      (vr/wrap-isearch-fun #'isearch-backward regexp))

    (defun vr/isearch-forward (&optional regexp)
      (interactive "P")
      (vr/wrap-isearch-fun #'isearch-forward regexp))

    (defun vr/isearch-backward-regexp (&optional regexp)
      (interactive "P")
      (vr/wrap-isearch-fun #'isearch-backward-regexp regexp))

    (defun vr/isearch-forward-regexp (&optional regexp)
      (interactive "P")
      (vr/wrap-isearch-fun #'isearch-forward-regexp regexp))

    :bind (("C-r" . vr/isearch-backward)
           ("C-s" . vr/isearch-forward)
           :map esc-map
           ("C-r" . vr/isearch-backward-regexp)
           ("C-s" . vr/isearch-forward-regexp))))
