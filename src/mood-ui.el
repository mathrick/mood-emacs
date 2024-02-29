;; -*- lexical-binding: t; nameless-current-name: "mood" -*-

;; mood-ui.el --- Mood, the modular Emacs config that isn't Doom

;; Copyright (C) 2020  Maciej Katafiasz

;; Author: Maciej Katafiasz <mathrick@gmail.com>
;; Keywords: convenience, extensions


;;; Commentary:
;; User-facing functions that are not required for Mood to function
;;

(require 'mood-core)

;;; Code:

(defun mood--interactive-prompt-module ()
  "Prompt user for a symbolic module path (eg. :lang/python), and
return a list of (section module path origin)"
  (let* ((completions (loop for candidate in (mood-known-modules)
                            for (section module where origin) = candidate
                            collect (cons (format "%s/%s" section module) candidate)))
         (selection (cdr (assoc-string (completing-read "Find module: " completions
                                                        nil t (try-completion "" completions))
                                       completions)))
         (candidates (remove-if-not (lambda (x)
                                      (equal x (subseq selection 0 2)))
                                    completions
                                    :key (lambda (x) (subseq x 1 3))))
         (candidates (append candidates candidates)))
    (if (and current-prefix-arg
             (> (length candidates) 1))
        (let* ((paths (mapcar (lambda (cand)
                                (list (abbreviate-file-name (elt cand 3)) cand))
                              candidates)))
          (second (assoc-string (completing-read "Location: " paths nil t)
                                paths)))
      selection)))

(defun mood-open-module-dir (section module path origin)
  "Interactively open the directory in which a module is
located. With prefix argument, also query for the location to
look in (see `*mood-module-paths*') if more than one is possible,
to find system modules being shadowed by user modules."
  (interactive (mood--interactive-prompt-module))
  (find-file path))

(defun mood-create-user-config-file (file &optional openp)
  "Create user config from template, write it to FILE. If OPENP
  is given, `find-file' will be called on the created file.

WARNING: This will *overwrite* existing files, see
`mood-maybe-create-user-config' and `mood-open-user-config' for a
safer, interactive alternative that will prompt the user to
create the config if missing."
  (let ((user-config-template (join-path *mood-checkout-root* "config.example.el")))
    (save-excursion
      (with-temp-file file
        (insert-file-contents user-config-template))))
  (when openp
    (find-file file)))

(defun mood-maybe-create-user-config (&optional noprompt openp)
  "Prompt the user to create config.el from template if it doesn't exist."
  (let ((user-config-file (join-path user-emacs-directory "config.el")))
    (when (and (not (file-exists-p user-config-file))
               (or noprompt
                   (y-or-n-p "config.el doesn't exist, create it from template?")))
      (mood-create-user-config-file user-config-file openp)
      (message "Use M-x mood-reload to apply your settings after editing"))))

(defun mood-open-user-config (&optional force)
  "Open the user's config.el. If it doesn't exist, create it from
  the template file in the upstream repo.

With a prefix argument, query the user to overwrite the existing
config.el with the template. With double prefix argument,
overwrite it without prompting"
  (interactive "p")
  (let ((force (and force (> force 1))) ; Single prefix arg
        (really-force (and force (> force 4))) ; Double prefix arg
        (user-config-file (join-path user-emacs-directory "config.el")))
    (if (file-exists-p user-config-file)
        (cond
         (really-force (mood-create-user-config-file user-config-file t))
         (force (if (yes-or-no-p "config.el already exists, overwrite with the template?")
                    (mood-create-user-config-file user-config-file t)
                  (find-file user-config-file)))
         (t (find-file user-config-file)))
      (mood-maybe-create-user-config force t))))

(defun mood-reload ()
  "Read the init file again after changing config. This will
apply new and changed settings, but it will not unload old
config, so a restart of Emacs might be necessary."
  (interactive)
  (load user-init-file))

(defun mood--extract-named-forms (symbols body)
  "Given a list of SYMBOLS, extract all matching forms from BODY.

BODY will be macroexpanded, so all places in which SYMBOLS are
called, even indirectly, will be collected. A form matches given
symbol if it's of the form (SYMBOL . REST) (ie. a function or
macro call). The return value is a list of all matching forms, in
order in which they appear in BODY (traversed
depth-first). Matching forms will be returned in their original
shape, ie. without any macroexpansion done."

  (let* ((syms-to-fresh (cl-loop for sym in symbols
                                 collect (cons sym (make-symbol (format "--%s--" sym)))))
         (fresh-to-syms (cl-loop for (sym . fresh) in syms-to-fresh
                                 collect (cons fresh sym)))
         (expanders (cl-loop for sym in symbols
                             ;; Create macrolet definition for each SYMBOL which simply expands to a
                             ;; quoted copy of itself with a fresh name. Since the names are fresh
                             ;; symbols, we can safely pick them out after macroexpansion, and
                             ;; quoting prevents further macroexpansion of the form.
                             collect `(,sym (&rest rest)
                                            `(quote (,',(cdr (assq sym syms-to-fresh)) ,@,'rest)))))
         ;; Now we simply macroexpand everything. Since
         (expanded (macroexpand-all `(macrolet (,@expanders)
                                       ,body))))
    ;; Finally, collect everything into the final list to be returned, mapping back from fresh names
    ;; to original
    (cl-labels ((traverse (form acc)
                          (pcase form
                            ;; If we see one of our fresh symbols, we know it was originally a form
                            ;; we were interested in, so collect it under the original name
                            ((and `(,fresh . ,rest)
                                  (guard (assq fresh fresh-to-syms)))
                             (cons `(,(cdr (assq fresh fresh-to-syms)) ,@rest) acc))
                            ;; For any other list, descend into head, then tail
                            (`(,head . ,tail)
                             (traverse tail (traverse head acc)))
                            ;; If not a list, just return accumulator as-is
                            (_ acc))))
      (reverse (traverse expanded ())))))

(defun mood--extract-module-flags (forms)
  "Given FORMS of Mood module's packages.el, extract feature flags it uses.
Feature flags are the switches mentioned in the (`feature!' ...) macro. Only
local flags will be returned, that is, ones using the short form
\(feature! +foo) of the macro. Explicit flags using the long form
\(feature! :section :module +flag) will not be. Extraction does not execute
the module's code.

The returned list will contain lists as its elements, coming from the
`mood--parse-switch-flag' function. Although any given parsed list will
only appear once in the result, a given flag might appear multiple times,
if it was parsed from different forms, e.g.
\(feature! +foo)
\(feature! -foo)
;; => ((:foo nil +) (:foo t -))

It is the caller's responsibility to ensure the list returned is
valid and self-consistent (see `mood--normalise-extracted-flags')"
  (cl-loop with flags = ()
           for form in (mood--extract-named-forms '(feature!) forms)
           do (pcase form
                (`(feature! ,flag)
                 (pushnew (mood--parse-switch-flag flag) flags :test #'equal)))
           finally return flags))

(defun mood--normalise-extracted-flags (path flags)
  "Normalise FLAGS returned by `mood--extract-module-flags'
PATH is the path of the file the flags were extracted from, and
is only used for context information if an error is signalled.

After normalising, the list is guaranteed to contain each flag
exactly once, including its default, direction and
documentation (if those were provided). If inconsistent flags
were present (e.g. +foo and -foo), an error will be signalled
instead"
  (cl-loop with seen = ()
           for decl in flags
           do (destructuring-bind (flag default direction) decl
                (destructuring-bind (&optional prev-default prev-direction) (plist-get seen flag)
                  ;; FIXME: This only really looks at the direction as the marker of uniqueness and
                  ;; consistency. default and doc could in principle differ, and we wouldn't know,
                  ;; but is it a real concern? It can't really happen when extracting flags from
                  ;; packages.el at the moment
                  (when (and direction
                             prev-direction
                             (not (eq direction prev-direction)))
                    (signal 'wrong-type-argument `(,(format "Feature flag %s in module file `%s' is used with inconsistent shorthands: %s and %s"
                                                            flag path direction prev-direction))))
                  (when (or direction (not prev-direction))
                    (setf seen (plist-put seen flag (list default direction))))))
           finally return (cl-loop for (flag (def dir)) on seen by #'cddr collect `(,flag ,def ,dir nil))))

(defun mood--merge-flags (manifest-flags extracted-flags)
  "Combine manifest flags with extracted flags that aren't already in the manifest"
  (append manifest-flags (cl-loop for extracted in extracted-flags
                                  for (flag &rest _) = extracted
                                  if (not (assoc flag manifest-flags))
                                  collect extracted)))

(provide 'mood-ui)
;;; mood-ui.el ends here
