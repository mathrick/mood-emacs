;; -*- lexical-binding: t; nameless-current-name: "mood" -*-

;; mood-straight.el --- Mood, the modular Emacs config that isn't Doom

;; Copyright (C) 2020  Maciej Katafiasz

;; Author: Maciej Katafiasz <mathrick@gmail.com>
;; Keywords: convenience, extensions


;;; Commentary:
;; Mood infrastructure that specifically interacts with straight.el
;;

;;; Code:

(require 'mood-core)

(defconst *mood-upstream-straight-profile* 'mood
  "Name of the profile Mood uses for pinning packages in upstream modules")
(defconst *mood-upstream-overrides-straight-profile* 'mood-overrides
  "Name of the profile Mood uses for pinning packages in upstream modules")
(defconst *mood-user-straight-profile* 'mood-user
  "Name of the profile Mood uses for pinning packages in user-defined modules")

(defconst *mood-package-recipes-paths*
  `((,*mood-upstream-straight-profile* . ,(join-path *mood-checkout-root* "package-recipes.el"))
    (,*mood-user-straight-profile* . ,(join-path user-emacs-directory "package-recipes.el")))
  "File paths to local package recipes for Mood upstream, and
  user's local modules. If these files exist, they should contain
  a list of recipes to be registered with straight.el.

  By default, all recipes registered this way will inherit from
  straight's built-in recipes, so that small tweaks can be made
  easily. To disable that and register a standalone recipe,
  specify `:inherit nil' in the recipe")

(defconst *mood-package-lockfile-paths*
  `((,*mood-upstream-straight-profile* . ,(join-path *mood-checkout-root* "package-lockfile.el"))
    (,*mood-user-straight-profile* . ,(join-path user-emacs-directory "package-lockfile.el")))
  "File paths to package lockfiles that straight.el should be using")

(defun mood--ingest-recipes (file)
  "Read and return straight.el recipes from FILE"
  (let ((forms (when (file-exists-p file)
                 (mood--read-all-forms file))))
    (cl-loop for recipe in forms
             unless (and (listp recipe)
                         (plistp (cdr recipe)))
             do (signal 'wrong-type-argument
                         (format "In file '%s': `%s' is not a valid straight.el recipe"
                                 file recipe))
             for (pkg . props) = recipe
             unless (plist-member props :inherit)
             do (plist-put props :inherit t)
             collect (cons pkg props))))

(defun mood--register-all-recipes ()
  "Read upstream and user recipe files, and register them with straight.el"
  ;; Note; have to collect all recipes first in a separate loop,
  ;; because `cl-loop's expansion is buggy for nested iteration
  (cl-loop for recipe in (cl-loop for (profile . path) in *mood-package-recipes-paths*
                                  nconc (mood--ingest-recipes path))
           do (straight-register-package recipe)))

(defun mood--get-straight-profiles ()
  "Return value suitable for `straight-profiles'. containing all
of Mood's lockfiles (see `*mood-package-lockfile-paths*'). The
existing value of `straight-profiles' is ignored, *unless* it
already contains entries for profiles listed in
`*mood-package-lockfile-paths*', in which case those entries only
will be included as-is in the returned value"
  (cl-loop for recipe in *mood-package-lockfile-paths*
           for (profile . path) = recipe
           if (assoc profile straight-profiles)
           collect it
           else collect (cons profile path)))

(defun mood--straight-wrap-module-load-function (loader-fn origin)
  "Implementation of `*mood-wrap-module-load-function*' providing
  proper profile tracking with straight.el"
  (let ((straight-current-profile (case origin
                                    (:upstream *mood-upstream-straight-profile*)
                                    (:user *mood-user-straight-profile*)
                                    (t (signal 'wrong-type-argument `(,(format "`%s' is not a known module origin" origin)))))))
    (funcall loader-fn)))

(defun mood-straight-freeze-versions (&optional force)
  "Same as `straight-freeze-versions', but takes care to only
freeze the versions of packages requested by user-defined modules
or manually by the user, and not overwrite the upstream Mood
lockfiles. FORCE has the same meaning as in
`straight-freeze-versions'"
  (interactive "p")
  (let ((straight-profiles
         (assq-delete-all *mood-upstream-straight-profile* (copy-alist straight-profiles))))
    (straight-freeze-versions force)))

(provide 'mood-straight)
;;; mood-straight.el ends here
