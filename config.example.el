(init! :base                 ;; Base OS, Emacs and Mood config. You probably want all of them
       os-support            ;; Make working on Windows suck less

       :theme                ;; A fresh coat of paint
       ;zenburn              ;; The original dark theme
       doom-themes           ;; Great artists steal

       :ui                   ;; General appearance and behaviour
       defaults              ;; Things we can all agree make sense
       ;(defaults (:font "monofur for Powerline 10"))
       undo                  ;; Less confusing undo system
       ;(undo +fu +session)  ;; (undo-tree by default, but you can choose undo-fu)
       doom-modeline         ;; Shinier modeline
       (icomplete +vertical) ;; The unsurprising minibuffer completion

       :editing              ;; It's not an emacsitor!
       ;company              ;; It's dangerous to type alone
       smartparens           ;; Nobody likes counting 'em
       multiple-cursors      ;; Trust me, you want this

       :vcs                  ;; Git, Bazaar, Hg, and others
       ;magit                ;; Honestly, don't even bother with git otherwise

       :lang                 ;; Languages, of the programming kind
       elisp                 ;; This is Emacs, after all
       ;python               ;; And the flying circus
       )
