(unless (featurep! -windmove)
  (use-package windmove
    :defer t
    :config
    (let ((modifier (or (featurep! :modifier) 'meta)))
      (loop for (key func) in '((left windmove-left)
                                (right windmove-right)
                                (up windmove-up)
                                (down windmove-down))
            do (general-define-key (vector (list modifier key)) func)))))

