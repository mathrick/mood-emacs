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
return a list of (section module path)"
  (let* ((completions (loop for candidate in (mood-known-modules)
			    for (section module where) = candidate
			    collect (cons (format "%s/%s" section module) candidate)))
	 (selection (cdr (assoc-string (completing-read "Find module: " completions
							nil t (try-completion "" completions))
				       completions)))
	 (candidates (remove-if-not (lambda (x)
				      (equal x (subseq selection 0 2)))
				    completions
				    :key (lambda (x) (subseq x 1 3)))))
    (if (and current-prefix-arg
	     (> (length candidates) 1))
	(let* ((paths (mapcar (lambda (cand)
				(list (abbreviate-file-name (elt cand 3)) cand))
			      candidates)))
          (cdr (assoc-string (find (completing-read "Location: " paths nil t))
		             paths)))
      selection)))

(defun mood-open-module-dir (section module path)
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

(defun mood--extract-module-flags (forms)
  "Given FORMS of Mood module's packages.el, extract feature flags it uses.
Feature flags are the switches mentioned in the (`featurep!' ...) macro. Only
local flags will be returned, that is, ones using the short form
\(featurep! +foo) of the macro. Explicit flags using the long form
\(featurep! :section :module +flag) will not be

The returned list will contain lists as its elements, coming from the
`mood--parse-switch-flag' function. Although any given parsed list will
only appear once in the result, a given flag might appear multiple times,
if it was parsed from different forms, e.g.
\(featurep! +foo)
\(featurep! -foo)
;; => ((:foo nil +) (:foo t -))"
  (let ((tag (make-symbol "sentinel"))
        (accumulator ()))
    ;; This is admittedly somewhat hacky, but it's not performance-sensitive code, and doing it this
    ;; way means we don't have to do any code walking. What this code does is repeatedly macroexpand
    ;; the FORMS, with a local definition of `featurep!' which instead of expanding will throw the
    ;; flag it got to the catch established outside of the macroexpand. The outside code can then
    ;; push the flag to the accumulator, and start a new round of macroexpansion. The two-step
    ;; process is used because within macroexpand, the code doesn't seem to have access to the outer
    ;; scope, so it can't push to the accumulator directly. If the same flag is seen again, it will
    ;; be skipped, so that the next (featurep! ...) can be expanded. When no more unextracted
    ;; (featurep!) forms remain, the catch form will return NIL, so we know we're done
    (cl-loop with done = nil
             while (not done)
             do (let ((flag (catch tag
                              (macroexpand-all
                               `(macrolet ((featurep! (flag &rest full-spec)
                                                      (let ((flag (mood--parse-switch-flag flag)))
                                                        ;; Non-local features are not a part of the module's definition, so skip them
                                                        (unless (or full-spec (member flag ',accumulator))
                                                          (throw ',tag flag)))))
                                  ,@forms))
                              nil)))
                  (when flag (push flag accumulator))
                  (setf done (not flag))))
    accumulator))

(defun mood--normalise-extracted-flags (path flags)
  (cl-loop with seen = ()
           for decl in flags
           do (destructuring-bind (flag default direction) decl
                (destructuring-bind (&optional prev-default prev-direction) (plist-get seen flag)
                  (when (and direction
                             prev-direction
                             (not (eq direction prev-direction)))
                    (signal 'wrong-type-argument `(,(format "Feature flag %s in module file `%s' is used with inconsistent shorthands: %s and %s"
                                                            flag path direction prev-direction))))
                  (when (or direction (not prev-direction))
                    (setf seen (plist-put seen flag (list default direction))))))
           finally return (cl-loop for (flag (def dir)) on seen by #'cddr collect `(,flag ,def ,dir nil))))

(defun mood--merge-flags (manifest-flags extracted-flags)
  "Combine manifest with extracted flags that aren't already in the manifest"
  (append manifest-flags (cl-loop for extracted in extracted-flags
                                  for (flag &rest _) = extracted
                                  if (not (assoc flag manifest-flags))
                                  collect extracted)))

(defun mood--gen-module-help (section module path)
  "Workhorse helper for `mood-help-module'.
Return string Insert help for given module in current buffer"
  (let* ((manifest (mood--ingest-manifest section module))
         (module-path (join-path path "packages.el"))
         (extracted-flags (mood--normalise-extracted-flags module-path
                                                       (mood--extract-module-flags (mood--read-all-forms module-path)))))
    (destructuring-bind (&key flags autoloads) manifest
      (let* ((flags (mood--merge-flags flags extracted-flags))
             (help-lines (cl-loop for (flag default dir doc) in flags
                                  collect (format "  * %s: %s%s %s-- %s"
                                                  (if dir "Switch" "Parameter")
                                                  (or dir ":")
                                                  (keyword-or-symbol-name flag)
                                                  (if (not dir)
                                                      (format "(default %s) " default)
                                                    "")
                                                  (or doc "*undocumented*")))))
        (with-output-to-string
          (let ((header (format "Mood module :%s/%s" (keyword-or-symbol-name section) module)))
            (princ header)
            (terpri)
            (princ (make-string (length header) ?=))
            (terpri)
            (terpri)
            (if help-lines
                (progn
                  (princ "Flags: ")
                  (terpri)
                  (cl-loop for line in help-lines
                           do (princ line)
                           do (terpri)))
              (progn
                (princ "Module does not declare any flags")))))))))

(defun mood--gen-module-help-buffer-name (section module)
  (format "*Mood module help :%s/%s*" (keyword-or-symbol-name section) (keyword-or-symbol-name module)))

(defun mood-help-module (section module path)
  "Show help for given module. Interactively, prompt for the module.
  The help will contain at least all the feature flags the module
  accepts, and can be ehanced by documentation provided in its
  manifest"
  (interactive (mood--interactive-prompt-module))
  (let ((buffer (get-buffer-create (mood--gen-module-help-buffer-name section module))))
    (with-current-buffer buffer
      (setf buffer-read-only nil)
      (erase-buffer)
      (insert (mood--gen-module-help section module path))
      (setf buffer-read-only t))
    (display-buffer buffer)))

(provide 'mood-ui)
;;; mood-ui.el ends here
