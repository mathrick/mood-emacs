;; -*- lexical-binding: t; -*-

(defvar devdocs-major-mode-to-docs-alist
  '((emacs-lisp-mode . ("elisp"))
    (html-mode . ("html" "css"))
    (markdown-mode . ("markdown"))

    ;; Treesiter compat
    (python-base-mode . ("python~3.10"))
    (python-mode . python-base-mode)

    ;; Shell scripting
    ;; Ordinarily, this would be `sh-mode', but we don't actually know the shell until
    ;; `sh-set-shell' is called, which calls its own hook
    (sh-set-shell . #'devdocs--get-sh-mode)
    ;; Pseudo-mode, doesn't exist, but it's a value resolved by `devdocs--get-sh-mode'
    ;; when the shell selected is Bash
    (bash . ("bash"))
    (bash-ts-mode . bash))

  "Mapping from major mode names to Devdocs documents which should
be automatically associated with those modes. During Emacs
startup, hooks are set up for all the modes listed, which will
invoke `devdocs-ensure-and-setup-mode-docs' every time a buffer
of that mode is created. That function will interpret the
mappings according to the rules below.

The keys are names of major modes (or more accurately, any X for
which `X-hook' is defined and called when applicable). The values
are one of three possible things:

- A list of strings, which will be used as the value of
  `devdocs-current-docs' in the buffer being set up

- A symbol, which should be another key in the alist, whose value
  will be looked up and used instead. This allows one mode to
  delegate to another mode and use the same documentation as
  it. This is useful with e.g. `python-mode' and
  `python-base-mode'

- A function (i.e. #'foo), which will be called with one
 argument, the original mode requested, and the current buffer
 set to the buffer for which the hook was invoked, and which
 should return the value to use

Any value which is not a list of strings will be recursively
resolved, for an arbitrary number of steps, so that it's legal
for example for the value to be a function, which returns a symbol,
which names another mode in the alist, etc.")

(defun devdocs-ensure-and-setup-mode-docs (mode)
  "Ensure all docs for MODE are installed,then set devdocs to use
them in current buffer. If any documents are missing, they will
be installed asynchronously when Emacs is idle."
  ;; Devdocs will not be loaded the first time we run
  (require 'devdocs)
  (let ((buf (current-buffer))
        (docs (devdocs--get-desired-docs mode)))
    (cl-flet ((setup ()
                (with-current-buffer buf
                  (setq-local devdocs-current-docs docs))))
      (cl-loop for doc in docs
               append (condition-case ()
                          (progn
                            (devdocs--doc-title doc)
                            nil)
                        ;; We'll get an error if DOC is not installed, collect it
                        (user-error (list doc)))
               into missing
               finally do
               ;; Ensure variable capture
               (when docs
                 (let ((timer-func (lambda ()
                                     (cl-loop for doc in missing
                                              do (devdocs-install doc))
                                     (setup))))
                   (if missing
                       (progn
                         (message "Missing devdocs for %s scheduled for installation" (string-join missing ", "))
                         (run-with-idle-timer 3 nil timer-func))
                     (setup))))))))

(defun devdocs--get-sh-mode (&rest _)
  "Return the shell scripting flavour used in the current buffer."
  sh-shell)

(defun devdocs--get-desired-docs (mode)
  "Return the devdocs applicable to MODE as a list of strings, by looking at
`devdocs-major-mode-to-docs-alist'.

This function will look up MODE in
`devdocs-major-mode-to-docs-alist' and chase delegated docs,
meaning that special values are supported that might be given
instead of a list:

* A symbol means delegation to another mode named by the symbol,
 e.g. an entry of (PYTHON-MODE . PYTHON-BASE-MODE) says
 \"PYTHON-MODE should use the same docs as PYTHON-BASE-MODE\"

* A FUNCTION form (aka sharp quote, ie. #'foo) names a function
 "
  (cl-labels ((resolve (mode-name)
                (let ((docs (alist-get mode-name devdocs-major-mode-to-docs-alist)))
                  (pcase docs
                    ((and (pred listp)
                          (guard (cl-every #'stringp docs)))
                     docs)

                    ((pred symbolp)
                     (resolve docs))

                    (`(function ,func) (resolve (funcall func mode)))))))
    (resolve mode)))

(use-package devdocs
  :bind (("C-h D" . devdocs-lookup))
  :init
  (cl-loop for (mode . _docs) in devdocs-major-mode-to-docs-alist
           ;; Ensure variable capture
           do (let ((mode mode))
                (add-hook (intern (format "%s-hook" mode))
                          (lambda () (devdocs-ensure-and-setup-mode-docs mode))))))
