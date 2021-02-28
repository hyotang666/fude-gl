(defpackage :fude-gl-examples
  (:use :cl))

(in-package :fude-gl-examples)

;;;; HELLO-TRIANGLE

(fude-gl:defshader hello-triangle 330 (fude-gl:vertex)
  (:vertex () "gl_Position = vec4(vertex, 0.0, 1.0);")
  (:fragment ((|outColor| :vec4)) "outColor = vec4(1.0, 1.0, 1.0, 1.0);"))

(defparameter *triangle*
  (concatenate '(array single-float (*))
               (make-instance 'fude-gl:vertex :x 0.0 :y 0.5)
               (make-instance 'fude-gl:vertex :x 0.5 :y -0.5)
               (make-instance 'fude-gl:vertex :x -0.5 :y -0.5)))

(defun hello-triangle ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :x 100
                           :y 100
                           :w 800
                           :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ((hello-triangle *triangle*))
          (sdl2:with-event-loop (:method :poll)
            (:quit ()
              t)
            (:idle ()
              (sdl2:gl-swap-window win)
              (gl:draw-arrays :triangles 0 3))))))))
