;; -*- lexical-binding: t; -*-

(use-package corfu
  :config
  (global-corfu-mode))

(unless (or (feature! -icons)
            (not (feature! :system nil :gui)))
  ;; Shamelessly stolen from https://kristofferbalintona.me/posts/202202270056/
  (use-package kind-icon
    :after corfu
    :custom
    (kind-icon-use-icons t)
    (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
    (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
    (kind-icon-blend-frac 0.08)

    ;; `kind-icon' depends `svg-lib' which creates a cache directory that defaults to the
    ;; `user-emacs-directory'. Here, I change that directory to a location appropriate to
    ;; `no-littering' conventions, a package which moves directories of other packages to
    ;; sane locations.
    (svg-lib-icons-dir (no-littering-expand-var-file-name "svg-lib/cache/"))
    :config
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
    ))

(unless (feature! -doc)
  (use-package corfu-doc
    :after corfu
    :hook (corfu-mode . corfu-doc-mode)))
