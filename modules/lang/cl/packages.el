;; -*- lexical-binding: t; -*-

;; Roswell + SLY/SLIME setup helpers
(defun roswell-impl-dir ()
  (cl-flet ((sh (cmd)
                (replace-regexp-in-string "_" "-" (string-trim (shell-command-to-string cmd)))))
    (let ((dir (downcase (expand-file-name (concat "~/.roswell/impls/"
                                                   (string-join (mapcar #'sh '("uname -m" "uname"))
                                                                "/"))))))
      (when (file-exists-p dir) dir))))

(defun guess-roswell-implementations ()
  "Auto-detect Roswell-installed CL implementations, returning a
  list suitable as a value of `sly-lisp-implementations' or
  `slime-lisp-implementations'"
  (let ((root (roswell-impl-dir)))
    (when root ; Skip if no Roswell impls
      (append '((roswell-default ("ros" "-Q" "run" "--")))
              (cl-loop for file in (directory-files root)
                       if (and (file-directory-p (expand-file-name file root))
                               (not (string-prefix-p "." file)))
                       collect `(,(intern file) ("ros" "-L" ,file "-Q" "run" "--")))))))

(defun qlot-start-lisp (dir &optional impl)
  "Start SLY or SLIME through Qlot in given `dir'

If `impl' (a symbol) is given, look it up in `sly-lisp-implementations'
(or `slime-lisp-implementations') and use its command, otherwise use
`sly-default-lisp' / `slime-default-lisp'"
  (interactive
   (let* ((slime (feature! +slime))
          (impls (if slime
                     slime-lisp-implementations
                   sly-lisp-implementations))
          (default-lisp (if slime
                            slime-default-lisp
                          sly-default-lisp)))
     (list (read-directory-name "Start SLY+Qlot in directory: " default-directory)
           (if current-prefix-arg
               (intern (completing-read "Lisp implementation to use: " (mapcar #'car impls)
                                        nil nil nil nil default-lisp))
             default-lisp))))
  (let* ((ql-dir (or (locate-dominating-file dir "qlfile")
                     (error "No qlfile found in `%s' or its parent directories" dir)))
         (impl (or impl default-lisp))
         (cmdlist (if impl
                      (cadr (assq impl impls))
                    (list inferior-lisp-program)))
         ;; Ugly, but when running ros through qlot, it needs an extra "-S ." argument,
         ;; and I can't think of a better way to do this
         (args (destructuring-bind (head tail) (--split-with (not (string= it "ros")) cmdlist)
                 (-concat head (when tail `("ros" "-S" ,ql-dir)) (cdr tail))
                 cmdlist)))
    (sly-start :name (intern (format "%s+qlot" impl))
               :program "qlot" :program-args `("exec" ,@args) :directory ql-dir)))

(unless (feature! +slime)
  (use-package sly
    :config
    (when (feature! +roswell)
      (setf sly-lisp-implementations (guess-roswell-implementations))
      ;; Set default Roswell impl as the default lisp
      (setf sly-default-lisp 'roswell-default)))

  (unless (feature! -asdf)
    (use-package sly-asdf))
  (unless (feature! -readtables)
    (use-package sly-named-readtables))
  (unless (feature! -ql)
    (use-package sly-quicklisp))
  (unless (feature! -macrostep)
    (use-package sly-macrostep)))

(when (feature! +slime)
  (use-package slime
    :config
    (when (feature! +roswell)
      (setf slime-lisp-implementations (guess-roswell-implementations))
      ;; Set default Roswell impl as the default lisp
      (setf slime-default-lisp 'roswell-default))))
