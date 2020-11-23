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
(defvar *mood-elisp-root*
  (if (getenv "MOOD_ELISP_ROOT")
      (expand-file-name (concat (getenv "MOOD_ELISP_ROOT") "/"))
    (let* ((file load-file-name)
           (symlink (file-symlink-p file))
           (mydir (file-name-directory (or symlink file))))
      (if (and (file-exists-p (expand-file-name "../src/" mydir))
               (file-exists-p (expand-file-name "../modules/" mydir)))
          mydir
        (error "mood.el must be located in subdirectory src/ of a checkout. Use (load) in your init file"))))
  "Directory in which mood.el resides. Should be a checkout of the Mood repo")

;; Ensure we have our core helpers
(load (concat *mood-elisp-root* "core-lib"))
(require 'core-lib)
(require 'cl)

(defvar *mood-module-paths*
  (list (file-name-as-directory (join-path user-emacs-directory "modules"))
        ;; This tedious dance of (file-name-directory (directory-file-name)) is
        ;; how you get the parent dir reliably in elisp :\
        (file-name-as-directory (join-path (file-name-directory
                                            (directory-file-name *mood-elisp-root*))
                                           "modules")))
  "List of directories where Mood modules can be found. Earlier
  entries take precedence over later ones, so upstream modules
  can be overriden by the user")

(defun mood--os-type ()
  (let ((os (cond
             ((string-equal "gnu/linux" system-type)  "linux")
             ((string-equal "darwin"    system-type)  "darwin")
             ((string-equal "windows"   system-type)  "windows")
             (t (warn (format "Mood: Unknown OS type %s" system-type))
                "unknown"))))
    (list :os (make-keyword os))))

(defun mood--feature-pred (feature pred)
  "Return `feature' if `pred' returns non-nil"
  (lambda ()
    (when (funcall pred)
     feature)))

(defconst *mood--feature-tests*
  (list #'mood--os-type
        ;; :interactive means we're not running in --batch mode
        (lambda () (when (or (not noninteractive) (daemonp))
                     :interactive))
        ;; GUI or terminal?
        (lambda () (if (display-graphic-p)
                       :gui
                     :terminal)))
  "List of functions to be run during initialisation. If a
  function returns non-nil value, it will be added to
  `*mood-feature-flags*'. If the value is a list, all its elements
  will be added to `*mood-feature-flags*'")

(defun mood-run-feature-tests ()
  (list :system
        (list nil (loop for test in *mood--feature-tests*
                        for flags = (funcall test)
                        append (if (listp flags)
                                   flags
                                 (list flags t)))
              'mood ())))

(defvar *mood-feature-flags* (mood-run-feature-tests)
  "Plist of plists representing features applicable to the
  current session. Used by modules to perform conditional
  initialisation.

  Each entry represents a section, containing modules,
  represented the same symbol as the module's name, and for each
  of them containing a plist of flags passed into the
  module. Each flag is a symbol. For simple flags, representing
  presence/absence of an option, the associated value should just
  be t. By convention, flags that specify the addition of
  something are named with leading + (ie. +lsp), whereas flags
  specifying the removal of something start with
  - (ie. -default-keybindings).

  For feature flags with multiple possible choices (such as
  OS type), the associated value represents the actual option. A
  value of nil is equivalent to the flag not being present at
  all.

  Every section can contain flags for a special pseudo-module
  `nil'. There will never be a module with that name, but it can
  be used to store flags common to the whole section. In
  particular, the pseudo-module :system/nil represents global
  feature flags, describing the running Emacs instance and
  general config, such as the OS, graphical display, etc. In
  addition to that, :system/mood might contain flags describing
  Mood-specific configuration.")

(defvar *mood-current-module* nil
  "Used by `mood-feature' when the short form with no specified
category and module is used.

Bound to the current module path (that is, list of (:category
module)) when a module's definition is being evaluated.")

(cl-defun mood--parse-module-spec (section sectionp module modulep)
  (unless (or (not (or modulep sectionp))
              (and modulep sectionp))
    (signal 'wrong-number-of-arguments
            '("Either only flag, or all of section/flag/module must be given")))
  (if sectionp
      (list section module)
    (or *mood-current-module*
        (signal 'wrong-number-of-arguments
                '("Short form feature spec used, but *mood-current-module* is not bound")))))

(cl-defun mood-feature-get (flag
                    &key (section nil sectionp) (module nil modulep)
                    error-if-missing)
  "Return the value of FLAG in SECTION and MODULE (see
  `*mood-feature-flags*'), or `nil' if not present. If only FLAG
  is given, it is looked up in the current module (as given by
  `*mood-current-module*').

  If ERROR-IF-MISSING is given, the `args-out-of-range' error will
  be signalled if the flag isn't present, instead of returning
  `nil'"
  (destructuring-bind (section module) (mood--parse-module-spec section sectionp
                                                            module modulep)
    (or (car (plist-member (plist-get (plist-get *mood-feature-flags* section) module) flag))
        (when error-if-missing
          (signal 'args-out-of-range
                  (list (format "Flag %s/%s not present" section module)))))))

(cl-defun mood-feature-put (flag
                        &key (section nil sectionp) (module nil modulep)
                        (value t))
  "Setter counterpart of `mood-feature-get'"
  (destructuring-bind (section module) (mood--parse-module-spec section sectionp
                                                            module modulep)
    (let* ((section-plist (plist-get *mood-feature-flags* section))
           (module-plist (plist-get section-plist module)))
      (setf *mood-feature-flags* (plist-put section-plist module
                                            (plist-put module-plist flag value))))))

(cl-defmacro featurep! (section-or-flag
                        &optional (section nil sectionp) (module nil modulep)
                        error-if-missing)
  "Convenience wrapper around `mood-feature-get' which takes
positional arguments and quotes them"
  `(mood-feature-get ',section-or-flag
             ,@(when sectionp (list :section `(quote ,section)))
             ,@(when modulep (list :module `(quote ,module)))))

(defun mood-load-module (section module &optional flags)
  "Load the specified MODULE from SECTION, adding FLAGS to
`*mood-features-flags*'. SECTION should be a keyword, MODULE might be
either a keyword or a plain symbol"
  (let* ((module-dir (concat (file-name-as-directory (keyword-or-symbol-name section))
                             (file-name-as-directory (keyword-or-symbol-name module))))
         (module-path (loop for path in *mood-module-paths*
                            for candidate = (concat path module-dir)
                            if (file-exists-p candidate)
                            return candidate
                            finally (error "Module %s/%s not found" section module)))
         (*mood-current-module* (list section module))
         (load-prefer-newer t))
    (when (file-exists-p (join-path module-path "packages.el"))
      (load (join-path module-path "packages")))))

(defvar *mood-no-init-straight* nil
  "If t, Mood will not initialise straight.el. Needs to be set before Mood is loaded")

(defvar *mood-straight-use-shallow-clone* t
  "If t (default), straight.el will be set up to use shallow git
  clones by default. This provides massive time and badwidth
  savings, especially during bootstrap")

(defun mood-init-straight ()
  "Initialise straight.el"
  (defvar bootstrap-version)
  (defvar straight-vc-git-default-clone-depth)
  (let* ((straight-vc-git-default-clone-depth
          (if *mood-straight-use-shallow-clone* 1 'full))
         (bootstrap-file
          (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
         (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

(defun mood--parse-init-block (args)
  (cl-flet ((no-sections (module)
                         (error "Module %s specified before any sections" module)))
    (loop with forms = ()
          with forms = ()
          with current-section = nil
          for arg in args
          do (cond
              ((keywordp arg)
               (setf current-section arg))
              ;; Just ignore nil entries
              ((eq arg nil))
              ;; (nil :foo :bar) isn't a real module, just a request to set
              ;; flags at the section level
              ((and (listp arg)
                    (eq (car arg) nil))
               (unless current-section (no-sections "<nil pseudo-module>"))
               (setf forms (append forms
                                   (loop for item in (cdr arg)
                                         collect (destructuring-bind
                                                     (flag &optional (value t))
                                                     (ensure-list item)
                                                   `(mood-feature-put ,flag
                                                                  :section ,current-section
                                                                  :module nil
                                                                  :value ,value))))))
              ((or (listp arg)
                   (symbolp arg))
               (destructuring-bind (module &rest flags) (ensure-list arg)
                 (unless current-section (no-sections module))
                 (setf forms (append forms
                                     (list `(mood-load-module ',current-section ',module
                                                          ,@flags)))))))
          finally return forms)))

;; This would ordinarily be a horrible hack, since we (read) from a file inside
;; a macro, but init code is a pretty special case and this makes it easier to
;; implement (init ...) whilst also hopefully making it easier to byte-compile
;; later
(defmacro mood--ingest-user-config (config-file)
  (let* ((config-file (eval config-file))
         (user-code (when (file-exists-p config-file)
                      (save-excursion
                        (with-temp-buffer
                          (insert-file-contents config-file)
                          (goto-char (point-min))
                          (loop for form = (condition-case nil
                                               (read (current-buffer))
                                             (end-of-file nil))
                                while form
                                collect form))))))
    `(macrolet ((init (&body body)
                      ;; Double backquotes are too annoying to deal with
                      (apply #'list 'progn (mood--parse-init-block body))))
       ,@user-code)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load user's config

(unless *mood-no-init-straight*
  (mood-init-straight))

(setq! straight-use-package-by-default t)
(straight-use-package 'use-package)
(straight-use-package 'general)

(mood--ingest-user-config (join-path user-emacs-directory "config.el"))

(provide 'mood)
;;; mood.el ends here
