(init! :base                            ; Base OS, Emacs and Mood config. You probably want all of them
       defaults                         ; You want them
       os-support                       ; Make working on Windows suck less

       :theme                           ; A fresh coat of paint
       doom-themes                      ; Great artists steal
       ;;zenburn                        ; The original dark theme

       :ui                              ; General appearance and behaviour
       mood                             ; Mood at your fingertips
       defaults                         ; Things we can all agree make sense
       ;;(defaults :font "monofur for Powerline 10")
       ;;auto-dim                       ; EXPERIMENTAL: I want to know where to look
       doom-modeline                    ; Shinier modeline
       ;;(icomplete +vertical)          ; The traditional minibuffer completion
       ;;selectrum                      ; Flexible minibuffer completion and narrowing
       vertico                          ; Like selectrum, but even simpler
       (scrolling +yascroll +smooth)    ; I held out for 20 years, but I'll finally admit it: Emacs scrollbars look hideous with any decent theme
       undo                             ; Less confusing undo system
       ;;(undo +fu +session)            ; (undo-tree by default, but you can choose undo-fu)
       windswap                         ; Like windmove, but also moves buffers

       :editing                         ; It's not an emacsitor!
       defaults                         ; Basic quality of life improvements
       company                          ; It's dangerous to type alone
       expand-region                    ; Make 'em bigger
       multiple-cursors                 ; Trust me, you want this
       realgud                          ; The unified debugger interface, MkII
       smartparens                      ; Nobody likes counting 'em
       visual-regexp                    ; Not for parsing HTML

       :vcs                             ; Git, Bazaar, Hg, and others
       magit                            ; Honestly, don't even bother with git otherwise

       :checkers                        ; Trust, but verify
       syntax                           ; Get squigglies when programming

       :lang                            ; Languages, of the programming kind
       (elisp +nameless)                ; This is Emacs, after all
       ;;org                            ; The all-singing, all-dancing organiser
       ;;python                         ; And the flying circus
       ;;yaml                           ; The most complicated simple language known to man
       )
