"Common Lisp support, using either SLY (default) or SLIME"

(defflag +slime "Use SLIME, rather than SLY")

(defflag -macrostep "SLY only: Don't load macrostep contrib")
(defflag -ql "SLY only: Don't load Quicklisp contrib")
(defflag -readtables "SLY only: Don't load named readtables contrib")
(defflag -asdf "SLY only: Don't load ASDF contrib")
(defflag +roswell "Don't try to use Roswell for default Lisp implementation")
