"Contains recommended, default UI settings provided by Mood.

These defaults are not mandatory, and can be individually
disabled, but other modules might make assumption which will
potentially require further tweaking if so."

(defflag :font nil "If present, font spec to use as default font, e.g. \"monofur 10\"")
(defflag :windmove 'meta "Windmove prefix key, nil to disable")
(defflag :cua t "Whether to use CUA-mode: t to use, nil to disable, 'keys to use with C-x/C-c/C-v/C-z")
(defflag +toolbar "Don't disable the toolbar when running in GUI mode")
(defflag -menubar "Disable the menu bar")
(defflag -avoid "Disable `mouse-avoidance-mode'")
(defflag -which-key "Disable `which-key-mode'")
(defflag -helpful "Disable Helfpul and use built-in Emacs help functions")
(defflag -icons "Don't set up rich icons. This might disable some modules which rely on them")
(defflag -ibuffer "Don't enable `ibuffer' by default, use plain `list-buffers'")
(defflag -text-scale-mode "Don't enable `default-text-scale-mode'")
