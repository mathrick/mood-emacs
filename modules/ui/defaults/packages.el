;; -*- lexical-binding: t; -*-

(let ((font (feature! :font)))
  (when (and font
             (not (eq font t))
             (feature! :system nil :gui))
    (set-frame-font font t t)))

;; Be kind, give the user a chance to back out of this incredibly easy
;; to press by accident key combo
(setq! confirm-kill-emacs (quote y-or-n-p))

(let ((modifier (feature! :windmove)))
  (when modifier
    (use-package windmove
     :defer t
     :init
     (loop for (key func) in '((left windmove-left)
			       (right windmove-right)
			       (up windmove-up)
			       (down windmove-down))
	   do (general-define-key (vector (list modifier key)) func)))))

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
(unless (feature! -cua)
  ;; Short form of (feature! ...) won't work by the time :config
  ;; runs, so save the value here
  (let ((keys (eq (feature! :cua) 'keys)))
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

(unless (feature! +toolbar)
  (setq! tool-bar-mode nil))

(unless (feature! -menubar)
  (setq! menu-bar-mode t))

;; Make sure the mouse cursor is not in the way
(when (and (feature! :system nil :gui)
           (not (feature! -avoid)))
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

;; Make it so that <pgup> immediately followed by <pgdown> returns to
;; the starting position, which is not Emacs's default. Arguably this
;; could go into :ui/scrolling, but the default behaviour is so
;; confusing, I think the fix belongs here
(setq! scroll-preserve-screen-position t)

;; None of these commands are any more "confusing" than usual, and all
;; are immensely useful, so let's enable them
(dolist (command '(narrow-to-region
                   upcase-region
                   downcase-region
                   set-goal-column))
        (put command 'disabled nil))

;; Sort directories first in dired
(if (featurep 'ls-lisp)
    (setq! ls-lisp-dirs-first t)
  (setq! dired-listing-switches
	 (format "%s --group-directories-first" dired-listing-switches)))

(use-package wdired
  :general ('dired-mode-map
            "r" #'wdired-change-to-wdired-mode))

;; Show a nice cheat sheet for available keys
(unless (feature! -which-key)
  (use-package which-key
    :config (which-key-mode)))

;; Nicer help functions
(unless (feature! -helpful)
  (use-package helpful
    :general
    ([remap describe-function] #'helpful-callable)
    ([remap describe-variable] #'helpful-variable)
    ([remap describe-key] #'helpful-key)

    ;; Hack to make helpful also kick in when using apropos. Simply
    ;; advising the global `describe-*' functions breaks things, so we
    ;; have to be more focused and verbose.
    ;;
    ;; See also https://github.com/Wilfred/helpful/issues/25
    :config
    (require 'apropos)
    (let ((do-function (lambda (button)
                         (helpful-callable (button-get button 'apropos-symbol))))
          (do-variable (lambda (button)
                         (helpful-variable (button-get button 'apropos-symbol)))))
      ;; :supertype only takes effect statically, at the time of
      ;; definition, so we can in fact redefine a button with itself
      ;; as its supertype
      (define-button-type 'apropos-function    :supertype 'apropos-function    'action do-function)
      (define-button-type 'apropos-macro       :supertype 'apropos-macro       'action do-function)
      (define-button-type 'apropos-command     :supertype 'apropos-command     'action do-function)
      (define-button-type 'apropos-variable    :supertype 'apropos-variable    'action do-variable)
      (define-button-type 'apropos-user-option :supertype 'apropos-user-option 'action do-variable))
    :init
    ;; Advice to ensure helfpul is loaded. Annoyingly, we can't make
    ;; it remove itself without making it way more complicated, so
    ;; let's just leave it in, it's really tiny
    (defun apropos-preload-helpful (&rest args)
      (require 'helpful))

    (loop for command in '(apropos apropos-command apropos-documentation
                           apropos-library apropos-local-value apropos-local-variable
                           apropos-user-option apropos-value apropos-variable)
          do (advice-add command :before #'apropos-preload-helpful))))

(unless (feature! -ibuffer)
  (use-package ibuffer
    :straight nil
    :general
    ([remap list-buffers] #'ibuffer)))

(unless (feature! -icons)
  (use-package nerd-icons
    :config
    ;; Try to detect automatically if the user needs to install the fonts
    (unless (or (find-font (font-spec :name nerd-icons-font-family))
                ;; If we're not in GUI mode, there's no way to detect fonts
                (not (feature! :system nil :gui)))
      (when (y-or-n-p "Icons are not set up. Would you like to download and install the required fonts?")
        (nerd-icons-install-fonts t))))
  ;; Compatibility wrappers that bridge any code expecting all-the-icons to nerd-icons
  (use-package all-the-icons)
  (use-package all-the-icons-nerd-fonts
    :after all-the-icons
    :config
    (all-the-icons-nerd-fonts-prefer)))
