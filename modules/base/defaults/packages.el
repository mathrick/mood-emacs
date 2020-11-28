;; -*- lexical-binding: t; -*-

;; By default, custom settings are simply discarded, since they only
;; serve to mess up the predictable results of what we're doing here
(if (featurep! +keep-custom)
    (progn
      (setq! custom-file (join-path user-emacs-directory "custom-settings.el"))
      (load-file custom-file))
  (setq! custom-file (case (featurep! :system nil :os)
                      (:windows "nul")
                      (t "/dev/null"))))

(unless (featurep! -file-history)
  ;; Remember the last position in previously visited files
  (use-package saveplace
    :config (save-place-mode t))

  (use-package recentf
    :config (recentf-mode)))

;; Remove the incredibly annoying and pointless warning about symlinks
;; to a VC-controlled file
(setq! vc-follow-symlinks t)
