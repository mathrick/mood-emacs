;; -*- lexical-binding: t; -*-

(let ((always-show-recent (featurep! +always-show-recent)))
  (use-package magit
    :defer t
    :config
    (when always-show-recent
      ;; Instead of showing *either* unpushed *or* recent, always show both
      (magit-add-section-hook 'magit-status-sections-hook
                              #'magit-insert-unpushed-to-upstream
                              #'magit-insert-unpushed-to-upstream-or-recent
                              'replace)
      (magit-add-section-hook 'magit-status-sections-hook
                              #'magit-insert-recent-commits
                              ;; Don't move it if it's already included (depends on magit version)
                              #'magit-insert-unpushed-to-upstream
                              t))))

(unless (featurep! -forge)
  (use-package forge
    :after magit))
