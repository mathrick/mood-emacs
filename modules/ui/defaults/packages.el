;; -*- lexical-binding: t; -*-

(let ((font (featurep! :font)))
  (when (and font
             (not (eq font t))
             (featurep! :system nil :gui))
    (set-frame-font font t t)))

(unless (featurep! -windmove)
  (use-package windmove
    :defer t
    :init
    (destructuring-bind (section module) *mood-current-module*
      (let ((modifier (or (featurep! :windmove-modifier) 'meta)))
        ;; FIXME: Store what we selected. Need better system for default values
        (mood-feature-put :section section :module module
                          :flag :windmove-modifier :value modifier)
        (loop for (key func) in '((left windmove-left)
                                  (right windmove-right)
                                  (up windmove-up)
                                  (down windmove-down))
              do (general-define-key (vector (list modifier key)) func))))))

;; CUA (https://en.wikipedia.org/wiki/IBM_Common_User_Access) mode
;; provides the CUA keys, as well as things like Shift-movement for
;; region operations. It is, by and large, an excellent mode that
;; makes Emacs far more consistent with almost all other editors,
;; without really taking away things that work in default
;; config. Therefore it's enabled by default.
;;
;; It also provides the standard Mac/Windows keys C-x/C-c/C-v/C-z,
;; which is commonly also referred to as "CUA", including by the CUA
;; mode, even though that is in fact incorrect. That isn't enabled by
;; default
(unless (featurep! -cua)
  ;; Short form of (featurep! ...) won't work by the time :config
  ;; runs, so save the value here
  (let ((keys (featurep! +cua-keys)))
    (use-package cua-base
     :defer 0
     :config
     (setq! cua-auto-tabify-rectangles nil)
     ;; CUA keys here refers to C-x/C-c/C-v/C-z,, which is a bit silly, so
     ;; the user needs to request that explicitly
     (setq! cua-enable-cursor-indications t)
     (setq! cua-enable-modeline-indications nil)
     (setq! cua-enable-region-auto-help t)
     (if keys
         (progn
           (setq! cua-enable-cua-keys t)
           (cua-mode nil))
       (cua-selection-mode nil)))))

(unless (featurep! +toolbar)
  (setq! tool-bar-mode nil))

(unless (featurep! -menubar)
  (setq! menu-bar-mode t))

;; Make sure the mouse cursor is not in the way
(when (and (featurep! :system nil :gui)
           (not (featurep! -avoid)))
  (use-package avoid
    :config
    (mouse-avoidance-mode 'exile)
    (setq! mouse-avoidance-nudge-dist 35)
    (setq! mouse-avoidance-threshold 15)))

(mouse-wheel-mode t)
(column-number-mode t)

;; Don't wrap long lines
(set-default 'truncate-lines t)
;; also in vertically split windows
(setq! truncate-partial-width-windows nil)

(unless (featurep! -file-history)
  ;; Remember the last position in previously visited files
  (use-package saveplace
    :config (save-place-mode t))

  (use-package recentf
    :config (recentf-mode)))

;; Show a nice cheat sheet for available keys
(unless (featurep! -which-key)
  (use-package which-key
    :config (which-key-mode)))

;; Nicer help functions
(unless (featurep! -helpful)
  (use-package helpful
    :general
    ([remap describe-function] #'helpful-callable)
    ([remap describe-variable] #'helpful-variable)
    ([remap describe-key] #'helpful-key)))
