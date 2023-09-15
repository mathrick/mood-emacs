;; -*- lexical-binding: t; -*-


(let ((cua (feature! :ui defaults :cua))
      (protect-meta (eq (feature! :ui defaults :windmove) 'meta))
      (disputed-keys (feature! :disputed-keys)))
  ;; Due to how :straight works, we need to disable it here so that
  ;; key replacements can occur before org is loaded, otherwise they
  ;; will have no effect
  (use-package org
    :init
    (when cua
      (setq org-support-shift-select 'always))
    (when (or cua protect-meta disputed-keys)
      (setq org-replace-disputed-keys t
            org-disputed-keys (loop for (key . replacement) in
                                    (or disputed-keys
                                        `(,@(when cua '(("S-<up>" . "M-p")
                                                        ("S-<down>" . "M-n")
                                                        ("S-<left>" . "M--")
                                                        ("S-<right>" . "M-+")
                                                        ("C-S-<right>" . "M-S-+")
                                                        ("C-S-<left>" . "M-S--")))
                                          ,@(when protect-meta
                                              '(("M-<left>" . "s-<left>")
                                                ("M-<right>" . "s-<right>")
                                                ("M-<up>" . "s-<up>")
                                                ("M-<down>" . "s-<down>")))))
                                    collect (cons (kbd key) (kbd replacement)))))))
