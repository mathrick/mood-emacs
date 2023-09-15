;; -*- lexical-binding: t; -*-

(unless (feature! +slime)
  (use-package sly)

  (unless (feature! -asdf)
    (use-package sly-asdf))
  (unless (feature! -readtables)
    (use-package sly-named-readtables))
  (unless (feature! -ql)
    (use-package sly-quicklisp))
  (unless (feature! -macrostep)
    (use-package sly-macrostep)))

(when (feature! +slime)
  (error "Module :lang/cl currently only supports SLY, not SLIME"))
