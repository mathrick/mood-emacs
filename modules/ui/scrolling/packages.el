;; -*- lexical-binding: t; -*-

(if (or (featurep! -scrollbar)
        (featurep! +yascroll))
    (scroll-bar-mode -1)
  (scroll-bar-mode 1))

(when (featurep! +yascroll)
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
(unless (featurep! -conservative)
  (use-package smooth-scrolling
    :config
    (setq smooth-scroll-margin 3)
    (smooth-scrolling-mode 1)))

;; OTOH, sublimity does provide true smooth (ie. animated) scrolling,
;; and is the least annoying and most usable implementation of
;; it. `pixel-scroll-mode', which is included in Emacs 26+, is
;; comically sluggish and thus entirely useless
(when (featurep! +smooth)
  (use-package sublimity-scroll
    :straight sublimity
    :config
    (setq! sublimity-scroll-weight 3
           sublimity-scroll-drift-length 5)
    ;; Guard against reload, since sublimity will take the value of
    ;; `auto-hscroll-mode', then unconditionally set it to `nil', so
    ;; on reload this would result in hscroll being disabled
    (setq! auto-hscroll-mode t)
    (sublimity-mode 1)))
