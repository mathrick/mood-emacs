;; -*- lexical-binding: t; -*-

(if (or (featurep! +disable)
        (featurep! +yascroll))
    (scroll-bar-mode -1)
  (scroll-bar-mode 1))

(when (featurep! +yascroll)
  (use-package yascroll
    :config
    (global-yascroll-bar-mode 1)
    (setq! yascroll:delay-to-hide 1)))

(let* ((minimap (featurep! +minimap))
       (side (if (eq minimap t)
                 'right
               minimap))
       (size (featurep! :size))
       (size (if (eq size t)
                 20
               size)))
  (when minimap
    (use-package minimap
      :config
      (setq! minimap-window-location side)
      (setq! minimap-width-fraction 0)
      (setq! minimap-minimum-width size)
      (minimap-mode))))
