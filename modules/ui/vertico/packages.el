;; -*- lexical-binding: t; -*-

(use-package vertico
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-indexed
                                vertico-flat
                                vertico-grid
                                vertico-mouse
                                vertico-quick
                                vertico-buffer
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform
                                vertico-unobtrusive))
  :config
  (vertico-mode))

(let ((style (feature! style))
      (separators (feature! extra-separators))
      (history (feature! history)))
  (ecase style
    ;; Nothing to do if the style is emacs, since that comes built-in
    (orderless
     (use-package orderless
       :config
       (when separators
         (setq! orderless-component-separator " +\\|[-/]"))))
    
    ((nil emacs)))

  (when history
    (ecase style
      (prescient
       (prescient-persist-mode +1))
      ((emacs orderless nil)
       (use-package historian
         :defer t
         :config
         (historian-mode +1))))))

(when (feature! +posframe)
  (use-package vertico-posframe
    :config
    (vertico-posframe-mode)
    :custom
    (vertico-posframe-poshandler #'posframe-poshandler-frame-top-center)))

;; FIXME: Split out into own packages / feature flags
(use-package vertico-directory
  :after vertico
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy) ; Correct file path when changed
  :bind (:map vertico-map
         ;; Vertico-directory which makes typing file paths in the minibuffer
         ;; more convenient. Use it to get a sense of what these do
         ("<backspace>"   . vertico-directory-delete-char)
         ("C-w"           . vertico-directory-delete-word)
         ("C-<backspace>" . vertico-directory-delete-word)))

(use-package marginalia
  :config (marginalia-mode))

(use-package nerd-icons-completion
  :after (marginalia nerd-icons)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :init
  (nerd-icons-completion-mode))
