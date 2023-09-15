;; -*- lexical-binding: t; -*-

(let* ((modifier (feature! :modifier))
       (follow (feature! :modifier))
       (base-modifier (when follow (list (feature! :ui defaults :windmove)))))
  (use-package windswap
    :defer t
    :init
    (when (or modifier base-modifier)
      (loop for (key func) in '((left windswap-left)
                                (right windswap-right)
                                (up windswap-up)
                                (down windswap-down))
            do (general-define-key (vector (append base-modifier (list modifier key)))
                                   func)))))
