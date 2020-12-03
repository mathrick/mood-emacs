;; mood-ui.el --- Mood, the modular Emacs config that isn't Doom -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Maciej Katafiasz

;; Author: Maciej Katafiasz <mathrick@gmail.com>
;; Keywords: convenience, extensions


;;; Commentary:
;; User-facing functions that are not required for Mood to function
;;

(require 'mood-core)

;;; Code:

(defun mood-open-module-dir (section module path)
  "Interactively open the directory in which a module is
located. With prefix argument, also query for the location to
look in (see `*mood-module-paths*') if more than one is possible,
to find system modules being shadowed by user modules."
  (interactive
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
	      (> (length candidates)
		 1))
	 (let* ((paths (mapcar (lambda (cand)
				 (list (abbreviate-file-name (elt cand 3)) cand))
			       candidates))
		(selection (cdr (assoc-string
				 (find (completing-read "Location: " paths nil t))
				 paths))))
	   selection)
       selection)))
  (find-file (join-path path
			(keyword-or-symbol-name section)
			(symbol-name module))))

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

(provide 'mood-ui)
;;; mood-ui.el ends here
