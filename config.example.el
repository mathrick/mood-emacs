(init! :base         ;; Base OS, Emacs and Mood config. You probably want all of them
       os-support    ;; Make working on Windows suck less

       :theme        ;; A fresh coat of paint
       zenburn       ;; The original dark theme

       :ui           ;; General appearance and behaviour
       defaults      ;; Things we can all agree make sense
       ;(defaults (:font "monofur for Powerline 10"))
       undo          ;; Less confusing undo system
       ;(undo +fu +session) ;; (undo-tree by default, but you can choose undo-fu)

       :editing      ;; Not an emacsitor!
       ;; company    ;; It's dangerous to type alone
       ;; smartparens   ;; Nobody like counting 'em

       :lang         ;; Languages, of the programming kind
       ;; python     ;; And the flying circus
       ;; (python +pyenv)
       )
