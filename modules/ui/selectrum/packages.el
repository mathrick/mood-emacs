;; -*- lexical-binding: t; -*-

(use-package selectrum
  :config
  (selectrum-mode +1))

(let ((style (featurep! style))
      (separators (featurep! extra-separators))
      (history (featurep! history)))
  (ecase style
    (emacs
     ;; Starting with raxod502/selectrum#330, `completion-styles' are
     ;; supported out of the box
     (if (fboundp 'selectrum-refine-candidates-using-completions-styles)
         (progn
           (setq! selectrum-refine-candidates-function #'selectrum-refine-candidates-using-completions-styles
                  selectrum-highlight-candidates-function #'selectrum-candidates-identity))
       ;; From https://github.com/raxod502/selectrum/issues/82
       (setq! selectrum-refine-candidates-function
              (lambda (input candidates)
                (nconc (completion-all-completions
                        input candidates nil (length input))
                       nil)))
       (setq! selectrum-highlight-candidates-function (lambda (_ candidates) candidates))))
    (orderless
     (use-package orderless
       :config
       (when separators
         (setq! orderless-component-separator " +\\|[-/]"))
       (setq! selectrum-refine-candidates-function #'orderless-filter)
       (setq! selectrum-highlight-candidates-function #'orderless-highlight-matches)))
    (prescient
     (use-package selectrum-prescient
       :defer t
       :config
       (selectrum-prescient-mode +1)))
    (nil))

  (when history
    (ecase style
      (prescient
       (prescient-persist-mode +1))
      ((emacs orderless nil)
       (use-package historian
         :defer t
         :config
         (historian-mode +1))))))
