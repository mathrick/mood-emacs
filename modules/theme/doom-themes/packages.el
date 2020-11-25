;; -*- lexical-binding: t; -*-

(let ((bold (not (featurep! -bold)))
      (italic (not (featurep! -italic)))
      (theme (or (featurep! :theme) 'doom-zenburn)))

  (use-package doom-themes
    :config
    ;; Global settings (defaults)
    (setq! doom-themes-enable-bold bold    ; if nil, bold is universally disabled
           doom-themes-enable-italic italic) ; if nil, italics is universally disabled
    (load-theme theme t)

    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)
    
    (when (and (featurep! :ui neotree :enabled)
               (featurep! :ui all-the-icons :enabled))
      ;; Enable custom neotree theme (all-the-icons must be installed)
      (doom-themes-neotree-config))
    
    (when (featurep! :ui treemacs :enabled)
      ;; or for treemacs users
      (setq doom-themes-treemacs-theme "doom-colors")
      (doom-themes-treemacs-config))
    
    (when (featurep! :editing org :enabled)
      ;; Corrects (and improves) org-mode's native fontification.
      (doom-themes-org-config))))
