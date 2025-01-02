;; -*- lexical-binding: t; -*-

;; This is rarely necessary on Linux these days, but still needed for
;; Windows
(set-language-environment "UTF-8")

;; By default, custom settings are simply discarded, since they only
;; serve to mess up the predictable results of what we're doing here
(if (feature! +keep-custom)
    (progn
      (setq! custom-file (join-path user-emacs-directory "custom-settings.el"))
      (load-file custom-file))
  (setq! custom-file (case (feature! :system//os)
                      (:windows "nul")
                      (t "/dev/null"))))

(unless (feature! -file-history)
  ;; Remember the last position in previously visited files
  (use-package saveplace
    :config (save-place-mode t))

  (use-package recentf
    :config
    (unless *mood-allow-litter*
      (add-to-list 'recentf-exclude no-littering-var-directory)
      (add-to-list 'recentf-exclude no-littering-etc-directory)
      (setq! auto-save-file-name-transforms
	     `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
      (unless (feature! -extra-litter)
        (no-littering-theme-backups)))
    (recentf-mode)))

;; Remove the incredibly annoying and pointless warning about symlinks
;; to a VC-controlled file
(setq! vc-follow-symlinks t)
