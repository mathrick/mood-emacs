;;; mood.el --- Mood, the modular Emacs config that isn't Doom -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Maciej Katafiasz

;; Author: Maciej Katafiasz <mathrick@gmail.com>
;; Keywords: convenience, extensions


;;; Commentary:
;; This is my attempt at creating my own modular config for Emacs, borne out of
;; the general admiration for mixed with the frustration at the specifics of
;; Doom.
;; 
;; 

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definitions

;; First things first, we need to get mood-core to load

(let* ((file load-file-name)
       (symlink (file-symlink-p file))
       (mydir (file-name-directory (or symlink file)))
       (core-file (expand-file-name "mood-core.el" mydir)))
  (if (file-exists-p core-file)
      (load core-file)
    (error "Could not load mood-core.el. mood.el must be located in subdirectory src/ of a checkout. Use (load) in your init file")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bootstrap all the necessary libraries

(unless *mood-no-init-straight*
  (mood-init-straight))

(setq! straight-use-package-by-default t)
(straight-use-package 'use-package)
(straight-use-package 'general)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; At this point, we should have everything defined, so let's load the
;; user's config

(mood--ingest-user-config (join-path user-emacs-directory "config.el"))
(mood-maybe-create-user-config nil t)


(provide 'mood)
;;; mood.el ends here
