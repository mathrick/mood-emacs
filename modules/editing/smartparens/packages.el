;; -*- lexical-binding: t; -*-

(let ((-strict (feature! -strict))
      (-parens (feature! -highlight))
      (style  (feature! :style)))
  (unless (memq style '(paredit sp))
    (error "Unknown smartparens style: %s" style))
  (use-package smartparens-config
    :straight smartparens
    ;; This is an ugly hack, but sp-override-key-bindings doesn't work
    ;; if set during init for some reason, so we're "deferring" it until
    ;; afterwards
    :defer 0
    :config
    (setq! sp-base-key-bindings style)
    ;; Unset SP's default keys that clash with other things we set
    (let* ((windmove (feature! :ui defaults :windmove))
           (cua (not (feature! :ui defaults -cua)))
           (unset `(,@(when windmove
                        ;; sp-override-key-bindings requires strings
                        ;; in (kbd) notation
                        (loop for key in '(up down)
                              collect (cons (key-description (vector (list windmove key)))
                                            nil)))
                    ,@(when cua
                        '(("C-<right>" . nil)
                          ("C-<left>"  . nil))))))

        (setq! sp-override-key-bindings
            (case style
              ('paredit
               `(,@unset

                 ("C-M-<right>" . sp-forward-sexp) ;; navigation
                 ("C-M-<left>"  . sp-backward-sexp)
                 ("C-M-<up>"    . sp-backward-up-sexp)
                 ("C-M-<down>"  . sp-down-sexp)
                 ("ESC <up>"    . sp-splice-sexp-killing-backward)
                 ("M-S-<up>"    . sp-splice-sexp-killing-backward)
                 ("ESC <down>"  . sp-splice-sexp-killing-forward)
                 ("M-S-<down>"  . sp-splice-sexp-killing-forward)
                 ("M-?"         . sp-convolute-sexp)

                 ("M-(" . (lambda ()
                            (interactive)
                            (sp-wrap-with-pair "(")))))
              ;; FIXME: do we need any extras like with paredit style?
              ('sp unset))))
    (if -strict
        (smartparens-global-mode)
      (smartparens-global-strict-mode))
    (unless (or (feature! :editing defaults -parens)
                -parens)
      (use-package smartparens-config
        :straight nil
        :config (show-smartparens-global-mode)))))
