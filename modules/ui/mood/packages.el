;; -*- lexical-binding: t; nameless-current-name: "mood"; -*-

(general-def
  :prefix-command 'mood-ui-map
  "M" '(mood-open-module-dir :wk "Find module dir")
  "P" '(mood-open-user-config :wk "Open personal config")
  "R" '(mood-reload :wk "Reload config")
  "H" '(mood-help-module :wk "Show help and configuration for given module"))

(general-def 'help-map
  "M" '(mood-ui-map :wk "Mood Emacs dashboard"))

(defvar mood-help-header-face 'bold-italic)

(defvar mood-help-section-face 'bold-italic)

(defvar mood-help-undocumented-face 'italic)

(defvar mood-help-special-face 'italic)

(defvar mood-help-identifier-face 'bold)

(defvar mood-help-basic-indent 2
  "Number of spaces a single level of indent takes")

(defvar *mood-help-current-indent-level* 0
  "Dynamic variable tracking the depth of current indentation")

(defun mood--format-line (line)
  (format "%s\n" line))

(defun mood--format-header (header)
  (format "%s\n%s\n"
          (mood--text-as-header header)
          (make-string (length header) ?=)))

(defun mood--indent-line (line)
  (format "%s%s"
          (make-string (* *mood-help-current-indent-level*
                          mood-help-basic-indent)
                       ? )
          line))

(defun mood--format-section (title lines)
  (format "%s\n%s\n%s"
          (mood--text-as-section title)
          (make-string (length title) ?-)
          (apply #'concat (cl-loop for line in lines
                                   with *mood-help-current-indent-level* = (1+ *mood-help-current-indent-level*)
                                   collect (mood--format-line (mood--indent-line line))))))

(defun mood--text-as-header (text)
  (propertize text 'face mood-help-header-face))

(defun mood--text-as-section (text)
  (propertize text 'face mood-help-section-face))

(defun mood--text-as-undocumented (text)
  (propertize text 'face mood-help-undocumented-face))

(defun mood--text-as-special (text)
    (propertize text 'face mood-help-special-face))

(defun mood--text-as-identifier (text)
    (propertize text 'face mood-help-identifier-face))

(defun mood--gen-module-help (section module path)
  "Workhorse helper for `mood-help-module'.
Return string Insert help for given module in current buffer"
  (let* ((manifest (mood--ingest-manifest section module))
         (module-path (join-path path "packages.el"))
         (extracted-flags (mood--normalise-extracted-flags module-path
                                                       (mood--extract-module-flags (mood--read-all-forms module-path)))))
    (destructuring-bind (&key flags autoloads description) manifest
      (let* ((flags (mood--merge-flags flags extracted-flags))
             (flag-help-lines (cl-loop for (flag default dir doc) in flags
                                       collect (format "* %s: %s %s-- %s"
                                                       (if dir "Switch" "Parameter")
                                                       (mood--text-as-identifier (format "%s%s"
                                                                                     (or dir ":")
                                                                                     (keyword-or-symbol-name flag)))
                                                       (if (not dir)
                                                           (format (mood--text-as-special "(default %s) ")
                                                                   (mood--text-as-identifier (format "%s" default)))
                                                         "")
                                                       (or doc (mood--text-as-undocumented "undocumented")))))
             (description (or description (mood--text-as-undocumented "No description provided"))))
        (concat (mood--format-header (format "Mood module :%s/%s" (keyword-or-symbol-name section) module))
                (mood--format-line "")
                (mood--format-line description)
                (mood--format-line "")
                (if flag-help-lines
                    (mood--format-section "Configuration flags" flag-help-lines)
                  (mood--text-as-undocumented "This module does not declare any configuration flags")))))))

(defun mood--gen-module-help-buffer-name (section module)
  (format "*Mood module help :%s/%s*" (keyword-or-symbol-name section) (keyword-or-symbol-name module)))

(defun mood-help-module (section module path origin)
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
