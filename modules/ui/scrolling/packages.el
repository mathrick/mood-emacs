;; -*- lexical-binding: t; -*-

(if (or (feature! -scrollbar)
        (feature! +yascroll))
    (scroll-bar-mode -1)
  (scroll-bar-mode 1))

(when (feature! +yascroll)
  (use-package yascroll
    :config
    (global-yascroll-bar-mode 2)
    (setq! yascroll:delay-to-hide 1)))

;; Despite its very misleading name, smooth-scrolling-mode has
;; *nothing* to do with what people generally mean when they say
;; "smooth scrolling", ie. animated scrolling by individual pixels
;; rather than whole lines. Rather, it's an extension of what
;; `scroll-conservatively' was meant to achieve, ie. make it so that
;; scrolling line by line to the bottom of the screen doesn't result
;; in huge disorienting jumps by screenfuls
(unless (feature! -conservative)
  (use-package smooth-scrolling
    :config
    (setq smooth-scroll-margin 3)
    (smooth-scrolling-mode 1)))

;; OTOH, good-scroll does provide true smooth (ie. animated)
;; scrolling, and is a usable implementation of it.
;; `pixel-scroll-mode', which is included in Emacs 26+, is
;; comically sluggish and thus entirely useless
(when (feature! +smooth)
  (use-package good-scroll
    :config
    (good-scroll-mode 1)))
