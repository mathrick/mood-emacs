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
  (require 'mood-straight)

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

  (mood-init-straight)

  ;; Default value includes `seq', but that and is wrong and breaks
  ;; compatibility of some packages on older Emacs versions
  (setf straight-recipes-gnu-elpa-ignored-packages
        (remq 'seq straight-recipes-gnu-elpa-ignored-packages))

  (setq! straight-use-package-by-default t)
  (mood-use-package 'use-package)

  ;; NB: Must use `mood-use-package' here and not `use-package',
  ;; because in Emacs 29 this`use-package' is built-in and tries to
  ;; macroexpand as the file is loaded, which defeats
  ;; straight-use-package-by-default and results in us not getting the
  ;; packages we expect
  (mood-use-package 'general)
  ;; Compat makes life easier when supporting multiple Emacs versions,
  ;; and is broadly used, so it's a dependency for us
  ;; as-well. Although it's built-in, by its very nature we need to
  ;; `use-package' it because versions corresponding to newer Emacs
  ;; versions that what we might be running on are distributed in GNU
  ;; ELPA and mood-straight.el requires those functions.
  (mood-use-package 'compat)

  (unless *mood-allow-litter*
    (mood-use-package 'no-littering))

  (require 'mood-keys)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; At this point, we should have everything defined, so let's load the
  ;; user's config

  ;; First, we need to set up things required for straight.el bookkeeping
  (setf straight-profiles (mood--get-straight-profiles))
  (setf *mood-wrap-module-load-function* #'mood--straight-wrap-module-load-function)
  (mood--register-all-recipes)

  (mood--ingest-user-config (join-path user-emacs-directory "config.el"))
  ;; Set the straight.el profile to user's, so that any package
  ;; locking they might do is correctly saved to their lockfile
  (setf straight-current-profile *mood-user-straight-profile*))


(require 'mood-ui)
(mood-maybe-create-user-config nil t)


(provide 'mood)
;;; mood.el ends here
