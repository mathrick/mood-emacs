;; core-lib.el -*- lexical-binding: t -*-

(defun make-keyword (str)
  "Similar to make-symbol, but returns a keyword, ie. an interned
symbol prepended with :"
  (declare (pure t) (side-effect-free t))
  (intern (concat ":" str)))

(defun ensure-list (thing)
  (if (listp thing)
      thing
    (list thing)))

(defun keyword-or-symbol-name (sym)
  (if (keywordp sym)
      (substring (symbol-name sym) 1)
    (symbol-name sym)))

(defun join-path (&rest components)
  (apply #'concat (append (mapcar #'file-name-as-directory
                                  (subseq components 0 -1))
                          (last components))))

;; Lifted from doom
(defmacro setq! (&rest settings)
  "A stripped-down `customize-set-variable' with the syntax of `setq'.
This can be used as a drop-in replacement for `setq'. Particularly when you know
a variable has a custom setter (a :set property in its `defcustom' declaration).
This triggers setters. `setq' does not."
  (macroexp-progn
   (cl-loop for (var val) on settings by 'cddr
            collect `(funcall (or (get ',var 'custom-set) #'set)
                              ',var ,val))))

(provide 'core-lib)
