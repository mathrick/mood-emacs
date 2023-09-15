;; -*- lexical-binding: t; -*-

(let ((minibuffer (not (feature! +minibuffer)))
      (background (or (feature! +minibuffer)
                      )))

  ;; https://oremacs.com/2015/04/28/blending-faces/
  (defun colir-join (r g b)
    "Build a color from R G B.
Inverse of `color-values'."
    (format "#%02x%02x%02x"
            (ash r -8)
            (ash g -8)
            (ash b -8)))

  (defun colir-blend (c1 c2 &optional alpha)
    "Blend the two colors C1 and C2 with ALPHA.
C1 and C2 are in the format of `color-values'.
ALPHA is a number between 0.0 and 1.0 which corresponds to the
influence of C1 on the result."
    (setq alpha (or alpha 0.5))
    (apply #'colir-join
           (cl-mapcar
            (lambda (x y)
              (round (+ (* x alpha) (* y (- 1 alpha)))))
            c1 c2)))

  (defun auto-dim-guess-faces ()
    (let ((fg (color-values
               (face-attribute 'default :foreground)))
          (bg (color-values
               (face-attribute 'default :background)))
          (darker (color-values "#000000")))
      `((((background light))
         :foreground ,(colir-blend fg bg 0.7)
         :background ,(colir-blend bg darker 0.95))
        (t
         :foreground ,(colir-blend fg bg 0.7)
         :background ,(colir-blend bg darker 0.7)))))

  (use-package auto-dim-other-buffers
    :hook (after-init .
                      (lambda ()
                        (face-spec-set 'auto-dim-other-buffers-face
                                       (auto-dim-guess-faces))))
    :config (auto-dim-other-buffers-mode t)))
