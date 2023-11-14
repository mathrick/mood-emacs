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

;; First things first, we need to get our load path to be able to find
;; the rest of Mood

(let* ((file load-file-name)
       (symlink (file-symlink-p file))
       (mydir (file-name-directory (or symlink file)))
       (core-file (expand-file-name "src/mood-core.el" mydir)))
  (if (file-exists-p core-file)
      (progn
        (add-to-list 'load-path  mydir)
        (add-to-list 'load-path (expand-file-name "src" mydir)))

    (error "Could not locate src/mood-core.el. mood.el must be located inside a checkout. Use a symlink or (load) in your init file")))

;; temporarily inhibit GC during startup for small to moderate time savings
(let ((gc-cons-threshold most-positive-fixnum))

  (require 'mood-core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Bootstrap all the necessary libraries

  ;; We need to set up no-littering before anything else, in fact
  ;; before we even load it, since straight.el should also use it
  (unless *mood-allow-litter*
    (defvar no-littering-var-directory (or (bound-and-true-p no-littering-var-directory)
                                           (expand-file-name "var/" user-emacs-directory))
      "Placeholder until we load no-littering")
    (defvar straight-base-dir no-littering-var-directory "Placeholder until we load straight.el")
    ;; Mood doesn't use package.el, but we still want to prevent its
    ;; droppings from being generated
    (setq package-user-dir (expand-file-name "elpa/" no-littering-var-directory)))

  (unless *mood-no-init-straight*
    (mood-init-straight))

  (setq! straight-use-package-by-default t)
  (straight-use-package 'use-package)

  (use-package general)

  (unless *mood-allow-litter*
    (use-package no-littering :demand t))

  (require 'mood-keys)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; At this point, we should have everything defined, so let's load the
  ;; user's config

  (mood--ingest-user-config (join-path user-emacs-directory "config.el")))

;; GC threshold restored to default

(require 'mood-ui)
(mood-maybe-create-user-config nil t)


(provide 'mood)
;;; mood.el ends here
