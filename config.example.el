(init! :base                            ; Base OS, Emacs and Mood config. You probably want all of them
       defaults                         ; You want them
       os-support                       ; Make working on Windows suck less

       :theme                           ; A fresh coat of paint
       doom-themes                      ; Great artists steal
       ;;zenburn                        ; The original dark theme

       :ui                              ; General appearance and behaviour
       mood                             ; Mood config at your fingertips
       defaults                         ; Things we can all agree make sense
       ;; (defaults :font "monofur for Powerline 10")
       ;; auto-dim                      ; EXPERIMENTAL: I want to know where to look
       modeline                         ; Shinier modeline
       ;; (icomplete +vertical)         ; The traditional minibuffer completion
       ;; selectrum                     ; Flexible minibuffer completion and narrowing
       vertico                          ; Like selectrum, but even simpler
       (scrolling +yascroll +smooth)    ; I held out for 20 years, but I'll finally admit it: Emacs scrollbars look hideous with any decent theme
       undo                             ; Less confusing undo system
       ;;(undo +fu +session)            ; (undo-tree by default, but you can choose undo-fu)
       windswap                         ; Like windmove, but also moves buffers

       :editing                         ; It's not an emacsitor!
       defaults                         ; Basic quality of life improvements
       ;;company                        ; It's dangerous to type alone
       corfu                            ; Corfu is to Company what Vertico is to Ivy
       expand-region                    ; Make 'em bigger
       edit-indirect                    ; Yo dawg, I heard you like buffers, so you can edit parts with a different major mode
       multiple-cursors                 ; Trust me, you want this
       realgud                          ; The unified debugger interface, MkII
       smartparens                      ; Nobody likes counting 'em
       visual-regexp                    ; Not for parsing HTML
       visual-fill                      ; I don't want my text as wide as my screen

       :vcs                             ; Git, Bazaar, Hg, and others
       magit                            ; Honestly, don't even bother with git otherwise

       :tools                           ; Various tools and utilities
       ;; vdiff                         ; What do you mean you don't like ediff?

       :checkers                        ; Trust, but verify
       syntax                           ; Get squigglies when programming

       :lang                            ; Languages, of the programming kind
       (elisp +nameless)                ; This is Emacs, after all
       cl                               ; Elisp's bigger brother everyone admires
       ;; clojure                       ; Elisp's cool younger sister
       emmet                            ; HTML that sucks a bit less
       ;; mood                          ; Not a real laguage, just helpers for writing Mood modules
       ;; org                           ; The all-singing, all-dancing organiser
       ;; python                        ; And the flying circus
       ;; yaml                          ; The most complicated simple language known to man
       )
