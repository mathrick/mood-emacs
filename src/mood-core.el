;; mood-core.el --- Mood, the modular Emacs config that isn't Doom -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Maciej Katafiasz

;; Author: Maciej Katafiasz <mathrick@gmail.com>
;; Keywords: convenience, extensions


;;; Commentary:
;; Definitions of core Mood functions and variables
;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definitions

(require 'core-lib)
(require 'cl)

(defvar *mood-elisp-root*
  (if (getenv "MOOD_ELISP_ROOT")
      (expand-file-name (concat (getenv "MOOD_ELISP_ROOT") "/"))
    (let* ((file load-file-name)
           (symlink (file-symlink-p file))
           (mydir (file-name-directory (or symlink file))))
      (if (and (file-exists-p (expand-file-name "../src/" mydir))
               (file-exists-p (expand-file-name "../modules/" mydir)))
          ;; Unexpand ~ in the returned dir
          (abbreviate-file-name mydir)
        (error "mood.el must be located in subdirectory src/ of a checkout. Use (load) in your init file"))))
  "Directory in which mood.el resides. Should be a checkout of the Mood repo")

(defvar *mood-checkout-root* (file-parent-directory *mood-elisp-root*)
  "Root directory of the Mood checkout, ie. the directory
  containing src/ and modules/")

(defvar *mood-module-paths*
  (loop for (dir . origin) in `((,user-emacs-directory . :user) (,*mood-checkout-root* . :upstream))
        collect (cons (join-path dir "modules") origin))
  "List of directories where Mood modules can be found. Earlier
  entries take precedence over later ones, so upstream modules
  can be overriden by the user")

(defvar *mood-wrap-module-load-function* nil
  "Variable that, if set, should contain a function of two
  arguments, which will be called for each module being loaded,
  and which should wrap the load operation with appropriate
  book-keeping. First argument passed in will be a closure, which
  should be called to perform the actual load operation. The
  second module will be the origin of the module, either
  `:upstream' or `:user', depending on whether the module being
  loaded was defined by Mood or the user respectively.

  Primarily intended for use by package manager's profile
  integration, ie. mood-straight.el")

(defun mood--os-type ()
  (let ((os (cond
             ((string-equal "gnu/linux" system-type)  "linux")
             ((string-equal "darwin"    system-type)  "darwin")
             ((string-equal "windows-nt"   system-type)  "windows")
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
  current session. Used by modules to determine which ones are
  enabled and perform conditional initialisation.

  Each entry represents a section, containing modules,
  represented the same symbol as the module's name, and for each
  of them containing a plist of flags passed into the
  module. Each flag is a keyword, however for simple flags,
  representing presence/absence of an option,
  `mood-feature-get/put' implement a special convention: flags
  that specify the addition or enabling of something are instead
  named with leading + (ie. +lsp), whereas flags specifying the
  removal or disabling of something start with -
  (ie. -default-keybindings) and its value is negated. In both
  cases the underlying flag is still stored and can be accessed
  as a keyword, the +/- convention is just a way of specifying
  the intention when accessing them (as well as providing a
  shorthand for manifesting default values in modules).

  Every section can contain flags for a special pseudo-module
  `nil'. There will never be a module with that name, but it can
  be used to store flags common to the whole section. In
  particular, the pseudo-module :system/nil represents global
  feature flags, describing the running Emacs instance and
  general config, such as the OS, graphical display, etc. In
  addition to that, :system/mood might contain flags describing
  Mood-specific configuration.")

(defvar *mood-feature-flag-defaults* ()
  "Stores default values for flags not present in
  `*mood-feature-flags*', as collected from module manifests")

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
      ;; Normalise section and module to plain symbol and keyword respectively (except for
      ;; nil). This allows us to be more lenient in what we accept, and notably makes the
      ;; shorthand notation easier to write
      (list (and section (make-keyword (keyword-or-symbol-name section)))
            (and module (intern (keyword-or-symbol-name module))))
    (or *mood-current-module*
        (signal 'wrong-number-of-arguments
                '("Short form feature spec used, but *mood-current-module* is not bound")))))

(defun mood--parse-shorthand (maybe-shorthand)
  "If MAYBE-SHORTHAND is of the form :foo/bar/baz, return its parts, split on /.
Any number of slashes (including zero) may be present, and the
return value is a list of one or more uninterned symbols, unless
the name is \"nil\", in which case nil will be returned. As a
special case, zero-length elements will also be returned as nil, ie.
\(mood--parse-shorthand :foo//bar) ;; => (foo nil bar)"
  (mapcar (lambda (x)
            (cond
             ((or (string-empty-p x)
                  (string= x "nil")) nil)
             ;; Note: we must treat keywords specially, since (keywordp (make-symbol ":foo")) => nil,
             ;; and that throws `keyword-or-symbol-name' off
             ((eq (elt x 0) ?:) (make-keyword (substring x 1)))
             (t (make-symbol x))))
          (split-string (keyword-or-symbol-name maybe-shorthand) "/")))

(defun mood--parse-switch-flag (flag)
  "If FLAG is a switch (ie. of the form +THING or -THING), return
its canonical name, default value and direction. The canonical
name is :THING, and the default value is nil for +THING and t for
-THING. Direction is either '+ or '-.

If FLAG is not a switch, ie. it's already a keyword or plain
symbol THING, just return its canonical name, with direction
being `nil'."
  (let ((name (keyword-or-symbol-name flag)))
    (cond
     ((string-prefix-p "+" name) (list (make-keyword (substring name 1))
                                       nil '+))
     ((string-prefix-p "-" name) (list (make-keyword (substring name 1))
                                       t '-))
     (t (list (make-keyword name) nil nil)))))

(cl-defun mood-feature-get (&key (section nil sectionp) (module nil modulep)
                             (flag nil flagp) error-if-missing)
  "Return the value of FLAG in SECTION and MODULE (see
  `*mood-feature-flags*'), or its default value (see
  `*mood-feature-flag-defaults*') if not present. If only FLAG is
  given, it is looked up in the current module (as given by
  `*mood-current-module*').

  FLAG can have one of three forms, which affect the default
  value and value returned:

  :THING - canonical form, default value is whatever the module
  manifest specifies (or `nil' if not manifested).

  +THING - same as :THING and is internally looked up as such, but
  signals that :THING is being accessed as an on/off switch,
  equivalent to asking \"is :THING enabled?\". Default value is
  `nil'.

  -THING - also internally looked up as :THING, but the returned
  value will be negated. Signals that :THING is being accessed
  as an on/off switch, equivalent to asking \"is :THING
  disabled?\". Default value is `t' (but remember it will be
  negated before being returned).

  If ERROR-IF-MISSING is given, the `args-out-of-range' error will
  be signalled if the flag isn't present, instead of returning
  the default value."
  (unless flagp
    (signal 'wrong-number-of-arguments '("flag is required")))
  (destructuring-bind (section module) (mood--parse-module-spec section sectionp
                                                            module modulep)
    (destructuring-bind (flag default dir) (mood--parse-switch-flag flag)
      (block outer
        (loop for plist in (list *mood-feature-flags* *mood-feature-flag-defaults*)
              do
              (let* ((found (plist-member (plist-get (plist-get plist section)
                                                     module)
                                          flag))
                     (value (second found)))
                (cond
                 ;; Negate the value of negative switches
                 ((and found (eq dir '-)) (return-from outer (not value)))
                 (found (return-from outer value))
                 (error-if-missing
                  (signal 'args-out-of-range
                          (list (format "Flag %s/%s not present" section module)))))))
        (if (eq dir '-)
            (not default)
          default)))))

(cl-defun mood-feature-put (&key (section nil sectionp) (module nil modulep)
                             (flag nil flagp) (value t))
  "Setter counterpart of `mood-feature-get'"
  (unless flagp
    (signal 'wrong-number-of-arguments '("flag is required")))
  (destructuring-bind (section module) (mood--parse-module-spec section sectionp
                                                            module modulep)
    (destructuring-bind (flag _default dir) (mood--parse-switch-flag flag)
      (let* ((section-plist (plist-get *mood-feature-flags* section))
             (module-plist (plist-get section-plist module)))
        (setf *mood-feature-flags*
              (plist-put *mood-feature-flags* section
                         (plist-put section-plist module
                                    (plist-put module-plist flag (if (eq dir '-)
                                                                     (not value)
                                                                   value)))))))))

(cl-defmacro feature! (section-or-flag
                        &optional (module nil modulep) (flag nil flagp)
                        error-if-missing)
  "Macro allowing Mood modules to query feature flags set by the user.
Two forms are supported:
;; Short form
\(when (feature! +foo) ...)

;; Long form, shorthand notation
\(when (feature! :ui/somemodule/-baz))
;; Long form, explicit notation; equivalent to the above
\(when (feature! :ui somemodule -baz) ...)

Short form accesses flags set by the current module during
loading process, and should be used by a module's definition to
determine user's preferences.

Long form is used only to access another module's flags, or
pseudo-modules such as :system/nil, to allow the module to provide
optional integration or adapt to the platform."
  (let* ((parsed-shorthand (mood--parse-shorthand section-or-flag))
         ;; This is ugly and repetitive, but cl-defmacro doesn't support
         ;; &whole, so we have to reassemble args manually and then repeat
         ;; the signature inside macrolet
         (args (append parsed-shorthand
                       (when modulep `(,module))
                       (when flagp `(,flag)))))
    `(macrolet ((doit (section-or-flag
                       &optional (module nil modulep) (flag nil flagp)
                       error-if-missing)
                      `(mood-feature-get ,@(when flagp `(:section ',section-or-flag))
                                     ,@(when modulep `(:module ',module))
                                     :flag ',(if flagp flag section-or-flag)
                                     :error-if-missing ,error-if-missing)))
       (doit ,@args))))

(defun mood-find-module (section module)
  (let ((module-dir (join-path (keyword-or-symbol-name section)
                               (keyword-or-symbol-name module))))
    (loop for (path . origin) in *mood-module-paths*
          for candidate = (join-path path module-dir)
          if (file-exists-p candidate)
          return (cons candidate origin)
          finally (error "Module %s/%s not found" section module))))

(defun mood-known-modules ()
  "Return the list of all modules that are known, ie. exist on
  the filesystem and would be a valid argument for
  `mood-find-module'. The return value is a list of four-element lists:

  ((:section1 module1 path origin) (:section2 module2 path origin) ...)

  where PATH is a subdirectory of an element of
  `*mood-module-paths*' where the module resides, and ORIGIN is
  one of `:upstream' or `:user', denoting whether it's a built-in
  Mood module, or a user-defined one. The same section and module
  can appear multiple times with different origins"
  (let ((valid-name-regexp (rx bol
                               (+ (or wordchar "-"))
                               eol)))
   (cl-flet ((valid-name-p (parent name)
                           (and (file-directory-p (join-path parent name))
                                (string-match-p valid-name-regexp name))))
     (loop for (path . origin) in *mood-module-paths*
           when (file-exists-p path)
           nconc
           (loop for section in (directory-files path)
                 for section-path = (join-path path section)
                 if (valid-name-p path section)
                 nconc
                 (loop for module in (directory-files section-path)
                       if (valid-name-p section-path module)
                       collect (list (make-keyword section)
                                     (intern module)
                                     (join-path section-path module)
                                     origin)))))))

(defun mood-load-module (section module &optional flags)
  "Load the specified MODULE from SECTION, adding FLAGS to
`*mood-features-flags*'. SECTION should be a keyword, MODULE
should be a plain symbol.

FLAGS, if set, should be a list of feature flags to pass to the
module (more exactly, to save in `*mood-features-flags*', under
the given module's key)."
  (let* ((module-info (mood-find-module section module))
         (module-path (car module-info))
         (origin (cdr module-info))
         (*mood-current-module* (list section module))
         (load-prefer-newer t)
         (wrap-fn (or *mood-wrap-module-load-function*
                      (lambda (loader-fn origin)
                        (funcall loader-fn)))))
    (loop for flag in flags
          do (destructuring-bind (flag &optional (value t)) (ensure-list flag)
               (mood-feature-put :section section :module module :flag flag :value value)))
    (let ((packages-file (join-path module-path "packages.el")))
      (when (file-exists-p packages-file)
        (funcall wrap-fn
                 (lambda () (load packages-file))
                 origin)))))

(defvar *mood-allow-litter* nil
  "If t, Mood will not load no-littering package and will not
  attempt to keep the user directory clean. Note that changing
  the value will not move existing files, so they will no longer
  be recognised and new ones will be generated.")

(defvar *mood-straight-use-shallow-clone* t
  "If t (default), straight.el will be set up to use shallow git
  clones by default. This provides massive time and badwidth
  savings, especially during bootstrap")

(defun mood-init-straight ()
  "Initialise straight.el"
  (defvar bootstrap-version)
  (defvar straight-vc-git-default-clone-depth (if *mood-straight-use-shallow-clone*
                                                  1
                                                'full))
  (let* ((straight-dir (or (bound-and-true-p straight-base-dir)
                           (join-path user-emacs-directory "straight/")))
         (bootstrap-file
          (join-path straight-dir "straight" "repos" "straight.el" "bootstrap.el"))
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
  (cl-labels ((no-sections (module)
                           (error "Module %s specified before any sections" module))
              ;; convert +foo to :foo t, etc. to make it easier to parse
              (normalise-flags (flags)
                               (loop for item in flags
                                     for valuep = nil then (not valuep)
                                     if valuep collect item
                                     else nconc
                                     (destructuring-bind (flag default dir)
                                         (mood--parse-switch-flag item)
                                       (if (not dir)
                                           (list item)
                                         (setf valuep t)
                                         (list flag (not default))))))
              (set-flags (flags section module)
                         (loop for (flag value) on (normalise-flags flags) by #'cddr
                               collect  `(mood-feature-put :section ,section
                                                       :module ',module
                                                       :flag ',flag
                                                       :value ,value))))
    (loop with preamble = ()
          with body = ()
          with modules = ()
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
               (push (set-flags (cdr arg) current-section nil)
                     preamble))
              ((or (listp arg)
                   (symbolp arg))
               (destructuring-bind (module &rest flags) (ensure-list arg)
                 (unless current-section (no-sections module))
                 (push (set-flags (append flags '(+enabled)) current-section module)
                       preamble)
                 (push `(mood-load-module ',current-section ',module)
                       body)
                 (push (list current-section module)
                       modules))))
          finally return (list
                          `(,@(mapcan #'identity (reverse preamble))
                            ,@(reverse body))
                          (reverse modules)))))

(defun mood--read-all-forms (file)
  (save-excursion
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (loop for form = (condition-case nil
                           (read (current-buffer))
                         (end-of-file nil))
            while form
            collect form))))

(defun mood--parse-manifest (manifest section module)
  (cl-flet ((normalise (spec)
                       (destructuring-bind (name &optional doc-or-default doc)
                           spec
                         (destructuring-bind (flag default-default dir
                                                   &aux (default (if dir
                                                                     default-default
                                                                   doc-or-default)))
                             (mood--parse-switch-flag name)
                           (if (and dir doc)
                               (error "defflag: switch flags take at most one argument")
                             (list flag default dir (if dir doc-or-default doc)))))))
    (loop for form in manifest
          with collected-flags = ()
          with collected-autoloads = ()
          with description = nil
          do (pcase form
               (`(defflag . ,spec) (push (normalise spec) collected-flags))
               (`(autoload . ,autoloads)
                (error "Autoloads not currently implemented")
                (push autoloads collected-autoloads))
               ((and (or `(description ,desc)
                         desc)
                     (guard (or (null desc) ;; Allow (description nil) placeholder form
                                (stringp desc))))
                (if description
                    (error "Manifest for %s/%s declares multiple description blocks" section module)
                  (setf description desc)))
               (_ (error "Unrecognised form %s in manifest for %s/%s" form section module)))
          finally return `(:flags ,(reverse collected-flags)
                           :autoloads ,(reverse (mapcan #'identity collected-autoloads))
                           :description ,description))))

(defun mood--ingest-manifest (section module &optional path)
  (let* ((module-dir (or path (car (mood-find-module section module))))
         (manifest-file (join-path module-dir "manifest.el"))
         (manifest (when (file-exists-p manifest-file)
                     (mood--read-all-forms manifest-file))))
    (mood--parse-manifest manifest section module)))

(defun mood--get-manifest (section module path)
  "Return the full, expanded manifest for MODULE.

In addition to what `mood--ingest-manifest' provides, used flags
  will be extracted from the module code, and placeholders will
  be provided for undocumented ones."
  (let* ((manifest (mood--ingest-manifest section module path))
         (module-path (join-path path "packages.el"))
         (extracted-flags (mood--normalise-extracted-flags module-path
                                                       (mood--extract-module-flags (mood--read-all-forms module-path)))))
    (destructuring-bind (&key flags autoloads description) manifest
      `(:flags ,(mood--merge-flags flags extracted-flags)
        :autoloads ,autoloads
        :description ,description))))

;; This would ordinarily be a horrible hack, since we `read' from a file inside
;; a macro, but init code is a pretty special case and this makes it easier to
;; implement (init! ...) whilst also hopefully making it easier to byte-compile
;; later
(defmacro mood--ingest-user-config (config-file)
  (let* ((config-file (eval config-file))
         (user-code (when (file-exists-p config-file)
                      (mood--read-all-forms config-file))))
    `(macrolet ((init! (&body body)
                       (destructuring-bind (load-forms modules)
                           (mood--parse-init-block body)
                         (let ((default-forms (loop for (section module) in modules
                                                    nconc
                                                    (destructuring-bind (&key flags autoloads description)
                                                        (mood--ingest-manifest section module)
                                                      (loop for (flag value dir _doc) in flags
                                                            collect  `(mood-feature-put :section ,section
                                                                                    :module ',module
                                                                                    :flag ',flag
                                                                                    :value ,value))))))
                           `(progn
                              ;; FIXME: Hacky, but I don't want to rewrite flag-put right now :\
                              (let ((*mood-feature-flags* *mood-feature-flag-defaults*))
                                ,@default-forms
                                (setf *mood-feature-flag-defaults* *mood-feature-flags*))
                              ,@load-forms)))))
       ,@user-code)))

(provide 'mood-core)
;;; mood-core.el ends here

;; Local Variables:
;; nameless-current-name: "mood"
;; End:
