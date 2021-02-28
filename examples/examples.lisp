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

;;;; UNIFORM-DEMO

(fude-gl:defshader uniform-demo 330 (fude-gl:vertex)
  ;; Re-use vertex-shader of HELLO-TRIANGLE.
  (:vertex () 'hello-triangle)
  (:fragment ((|outColor| :vec4) &uniform (|triangleColor| :vec3))
    "outColor = vec4(triangleColor, 1.0);"))

(defun uniform-demo ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :x 100
                           :y 100
                           :w 800
                           :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ((uniform-demo *triangle*))
          (let ((uniform-color
                 (gl:get-uniform-location uniform-demo "triangleColor")))
            (sdl2:with-event-loop (:method :poll)
              (:quit ()
                t)
              (:idle ()
                (sdl2:gl-swap-window win)
                (gl:uniformf uniform-color
                             (/ (+ 1.0 (sin (get-universal-time))) 2) 0.0 0.0)
                (gl:draw-arrays :triangles 0 3)))))))))

;;;; COLORED-TRIANGLE

(fude-gl:defshader colored-triangle 330 (fude-gl:vertex fude-gl:color)
  (:vertex ((|Color| :vec3))
    "Color = color;"
    "gl_Position = vec4(vertex, 0.0, 1.0);")
  (:fragment ((|outColor| :vec4)) "outColor = vec4(Color, 1.0);"))

(defparameter *colored-triangle*
  (concatenate '(array single-float (*))
               (make-instance 'colored-triangle
                              :x 0.0
                              :y 0.5
                              :r 1.0
                              :g 0.0
                              :b 0.0)
               (make-instance 'colored-triangle
                              :x 0.5
                              :y -0.5
                              :r 0.0
                              :g 1.0
                              :b 0.0)
               (make-instance 'colored-triangle
                              :x -0.5
                              :y -0.5
                              :r 0.0
                              :g 0.0
                              :b 1.0)))

(defun colored-triangle ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :x 100
                           :y 100
                           :w 800
                           :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ((colored-triangle *colored-triangle*))
          (sdl2:with-event-loop (:method :poll)
            (:quit ()
              t)
            (:idle ()
              (sdl2:gl-swap-window win)
              (gl:draw-arrays :triangles 0 3))))))))
