;; -*- lexical-binding: t; -*-

(general-def
  :prefix-command 'mood-ui-map
  "M" '(mood-open-module-dir :wk "Find module dir")
  "P" '(mood-open-user-config :wk "Open personal config")
  "R" '(mood-reload :wk "Reload config"))

(general-def 'help-map
  "M" '(mood-ui-map :wk "Mood Emacs dashboard"))

