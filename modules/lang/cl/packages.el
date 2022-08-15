;; -*- lexical-binding: t; -*-

(unless (featurep! +slime)
  (use-package sly)

  (unless (featurep! -asdf)
    (use-package sly-asdf))
  (unless (featurep! -readtables)
    (use-package sly-named-readtables))
  (unless (featurep! -ql)
    (use-package sly-quicklisp))
  (unless (featurep! -macrostep)
    (use-package sly-macrostep)))
