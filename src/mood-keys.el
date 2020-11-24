;;; mood-keys.el --- Mood, the modular Emacs config that isn't Doom -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Maciej Katafiasz

;; Author: Maciej Katafiasz <mathrick@gmail.com>
;; Keywords: convenience, extensions


;;; Commentary:
;; Functions for keymap manipulation and hackery
;; 

;;; Code:

;; This still needs work, don't user for now
(defun mood-defered-key-thunk (key command)
  (general-lambda ()
    (funcall-interactively command)
    (general-simulate-key key)))

(provide 'mood-keys)
;;; mood-keys.el ends here

;; Local Variables:
;; nameless-current-name: "mood"
;; End:
