;; -*- lexical-binding: t; -*-

(use-package edit-indirect
  :config
  (unless (feature! -autosave)
    (add-hook 'edit-indirect-after-commit-functions
              (lambda (beg end)
                (save-buffer)))))

(unless (feature! -autodetect)
  (use-package language-detection
    :config
    (defun edit-indirect-language-detection-guess-mode (parent-buffer beg end)
      "Guess the major mode from the PARENT-BUFFER substring from BEG to END using `language-detection.el'."
      (let* ((indirect-substring (with-current-buffer parent-buffer
                                   (buffer-substring-no-properties beg end)))
             (language (funcall #'language-detection-string indirect-substring))
             (mode (intern (format "%s-mode" language))))
        (if (fboundp mode)
            (funcall mode)
          (message "Mode %s detected, but isn't available." mode)
          (normal-mode)))))
  (setf edit-indirect-guess-mode-function #'edit-indirect-language-detection-guess-mode))
