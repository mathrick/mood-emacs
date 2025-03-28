;; -*- lexical-binding: t; -*-

(case (feature! :flavour)
  ('doom
   (unless (feature! :ui/defaults/icons)
     (error "doom-modeline requires rich icons, but :ui/defaults/icons flag has been turned off"))
   (use-package doom-modeline
           :config
           (doom-modeline-mode)))
  ('spaceline
   (use-package spaceline
     :config
     (require 'spaceline-config)
     (setf powerline-default-separator 'wave)
     (setf spaceline-highlight-face-func #'spaceline-highlight-face-modified)
     (spaceline-emacs-theme)))
  ('moody
   (use-package moody
     :config
     (setf x-underline-at-descent-line t)
     (moody-replace-mode-line-buffer-identification)
     (moody-replace-vc-mode)
     (moody-replace-eldoc-minibuffer-message-function))))
