(defpackage :fude-gl-examples
  (:use :cl)
  (:export #:demos))

(in-package :fude-gl-examples)

;;;; INTERFACE

(defun demos ()
  (restart-case (mapc #'funcall
                      '(hello-triangle uniform-demo colored-triangle
                                       element-buffer texture-demo mix-demo))
    (quit ())))

;;;; HELLO-TRIANGLE

(fude-gl:defshader hello-triangle 330 (fude-gl:xy)
  (:vertex () "gl_Position = vec4(xy, 0.0, 1.0);")
  (:fragment ((|outColor| :vec4)) "outColor = vec4(1.0, 1.0, 1.0, 1.0);"))

(defparameter *triangle*
  (concatenate '(array single-float (*))
               (make-instance 'fude-gl:xy :x 0.0 :y 0.5)
               (make-instance 'fude-gl:xy :x 0.5 :y -0.5)
               (make-instance 'fude-gl:xy :x -0.5 :y -0.5)))

(defun hello-triangle ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :x 100
                           :y 100
                           :w 800
                           :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ((hello-triangle
                                (:vertices *triangle*)
                                (:indices '(0 1 2))))
          (sdl2:with-event-loop (:method :poll)
            (:quit ()
              t)
            (:idle ()
              (fude-gl:with-clear (win (:color-buffer-bit))
                (fude-gl:draw-elements :triangles (fude-gl:indices-of
                                                   hello-triangle))))))))))

;;;; UNIFORM-DEMO

(fude-gl:defshader uniform-demo 330 (fude-gl:xy)
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
        (fude-gl:with-shader ((uniform-demo
                                (:vertices *triangle*)
                                (:indices '(0 1 2))
                                (:uniform |triangleColor|)))
          (sdl2:with-event-loop (:method :poll)
            (:quit ()
              t)
            (:idle ()
              (fude-gl:with-clear (win (:color-buffer-bit))
                (gl:uniformf |triangleColor|
                             (/ (+ 1.0 (sin (get-internal-real-time))) 2) 0.0
                             0.0)
                (gl:draw-arrays :triangles 0 3)))))))))

;;;; COLORED-TRIANGLE

(fude-gl:defshader colored-triangle 330 (fude-gl:xy fude-gl:rgb)
  (:vertex ((color :vec3)) "color = rgb;" "gl_Position = vec4(xy, 0.0, 1.0);")
  (:fragment ((|outColor| :vec4)) "outColor = vec4(color, 1.0);"))

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
        (fude-gl:with-shader ((colored-triangle
                                (:vertices *colored-triangle*)
                                (:indices '(0 1 2))))
          (sdl2:with-event-loop (:method :poll)
            (:quit ()
              t)
            (:idle ()
              (fude-gl:with-clear (win (:color-buffer-bit))
                (gl:draw-arrays :triangles 0 3)))))))))

;;;; ELEMENT-BUFFER

(defparameter *element-buffer-example*
  (concatenate '(array single-float (*))
               (make-instance 'colored-triangle ; Top left
                              :x -0.5
                              :y 0.5
                              :r 1.0
                              :g 0.0
                              :b 0.0)
               (make-instance 'colored-triangle ; Top right
                              :x 0.5
                              :y 0.5
                              :r 0.0
                              :g 1.0
                              :b 0.0)
               (make-instance 'colored-triangle ; Bottom right
                              :x 0.5
                              :y -0.5
                              :r 0.0
                              :g 0.0
                              :b 1.0)
               (make-instance 'colored-triangle ; Bottom left
                              :x -0.5
                              :y -0.5
                              :r 1.0
                              :g 1.0
                              :b 1.0)))

(defun element-buffer ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :x 100
                           :y 100
                           :w 800
                           :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ((colored-triangle
                                (:vertices *element-buffer-example*)
                                (:indices '(0 1 2 2 3 0))))
          (sdl2:with-event-loop (:method :poll)
            (:quit ()
              t)
            (:idle ()
              (fude-gl:with-clear (win (:color-buffer-bit))
                (fude-gl:draw-elements :triangles (fude-gl:indices-of
                                                   colored-triangle))))))))))

;;;; TEXTURE

(fude-gl:defshader texture-demo 330 (fude-gl:xy fude-gl:rgb fude-gl:st)
  (:vertex ((color :vec3) (texcoord :vec2))
    "texcoord = st;"
    "color = rgb;"
    "gl_Position = vec4(xy, 0.0, 1.0);")
  (:fragment ((|outColor| :vec4) &uniform (tex :|sampler2D|))
    "outColor = texture(tex, texcoord) * vec4(color, 1.0);"))

(defparameter *quad*
  (concatenate '(array single-float (*))
               (make-instance 'texture-demo ; Top left
                              :x -0.5
                              :y 0.5
                              :r 1.0
                              :g 0.0
                              :b 0.0
                              :s 0.0
                              :t 0.0)
               (make-instance 'texture-demo ; Top right
                              :x 0.5
                              :y 0.5
                              :r 0.0
                              :g 1.0
                              :b 0.0
                              :s 1.0
                              :t 0.0)
               (make-instance 'texture-demo ; Bottom right
                              :x 0.5
                              :y -0.5
                              :r 0.0
                              :g 0.0
                              :b 1.0
                              :s 1.0
                              :t 1.0)
               (make-instance 'texture-demo ; Bottom left
                              :x -0.5
                              :y -0.5
                              :r 1.0
                              :g 1.0
                              :b 1.0
                              :s 0.0
                              :t 1.0)))

(defparameter *png*
  (opticl:read-png-file
    (probe-file
      (merge-pathnames "examples/lisplogo_alien_128.png"
                       (asdf:system-source-directory
                         (asdf:find-system :fude-gl-examples))))))

(defun texture-demo ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :x 100
                           :y 100
                           :w 800
                           :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ((texture-demo
                                (:vertices *quad*)
                                (:indices '(0 1 2 2 3 0))
                                (:uniform (tex :texture-2d
                                           (fude-gl:tex-image-2d *png*)))))
          (sdl2:with-event-loop (:method :poll)
            (:quit ()
              t)
            (:idle ()
              (fude-gl:with-clear (win (:color-buffer-bit))
                (fude-gl:draw-elements :triangles (fude-gl:indices-of
                                                   texture-demo))))))))))

;;;; MIX

(fude-gl:defshader mix-demo 330 (fude-gl:xy fude-gl:st)
  (:vertex ((texcoord :vec2))
    "texcoord = st;"
    "gl_Position = vec4(xy, 0.0, 1.0);")
  (:fragment ((|outColor| :vec4) &uniform (tex1 :|sampler2D|)
              (tex2 :|sampler2D|))
    "outColor = mix(texture(tex1, texcoord),
                     texture(tex2, texcoord),
                     0.5);"))

(defparameter *mix-demo*
  (concatenate '(array single-float (*))
               (make-instance 'mix-demo :x -0.5 :y 0.5 :s 0.0 :t 0.0)
               (make-instance 'mix-demo :x 0.5 :y 0.5 :s 1.0 :t 0.0)
               (make-instance 'mix-demo :x 0.5 :y -0.5 :s 1.0 :t 1.0)
               (make-instance 'mix-demo :x -0.5 :y -0.5 :s 0.0 :t 1.0)))

(defparameter *logo*
  (opticl:read-png-file
    (probe-file
      (merge-pathnames "examples/lisplogo_warning2_128.png"
                       (asdf:system-source-directory
                         (asdf:find-system :fude-gl-examples))))))

(defun mix-demo ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :x 100
                           :y 100
                           :w 800
                           :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ((mix-demo
                                (:vertices *mix-demo*)
                                (:indices '(0 1 2 2 3 0))
                                (:uniform (tex1 :texture-2d
                                           (fude-gl:tex-image-2d *png*))
                                          (tex2 :texture-2d
                                           (fude-gl:tex-image-2d *logo*)))))
          (sdl2:with-event-loop (:method :poll)
            (:quit ()
              t)
            (:idle ()
              (sdl2:gl-swap-window win)
              (fude-gl:draw-elements :triangles (fude-gl:indices-of
                                                 mix-demo)))))))))

;;;; HELLO from glut-examples.

(fude-gl:defshader hello 330 (fude-gl:xy)
  (:vertex () "gl_Position = vec4(xy,0.0,1.0);")
  (:fragment ((color :vec4)) "color = vec4(1.0, 1.0, 1.0, 1.0);"))

(defparameter *hello-quad*
  (concatenate '(array single-float (*)) (make-instance 'hello :x -0.5 :y 0.5) ; top-left
               (make-instance 'hello :x 0.5 :y 0.5) ; top-right
               (make-instance 'hello :x -0.5 :y -0.5) ; bottom-left
               (make-instance 'hello :x 0.5 :y -0.5))) ; bottom-right

(defun hello ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :x 100
                           :y 100
                           :w 250
                           :h 250
                           :title "hello")
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ((hello
                                (:vertices *hello-quad*)
                                (:indices '(0 1 2 2 3 1))))
          (sdl2:with-event-loop (:method :poll)
            (:quit ()
              t)
            (:idle ()
              (fude-gl:with-clear (win (:color-buffer-bit))
                (fude-gl:draw-elements :triangles (fude-gl:indices-of
                                                   hello))))))))))

;;;; DOUBLE from glut-examples.

(fude-gl:defshader double 330 (fude-gl::xyz)
  (:vertex ((|texCoord| :vec2) &uniform (transform :mat4))
    "gl_Position = transform * vec4(xyz, 1.0);")
  (:fragment ((color :vec4)) "color = vec4(1.0, 1.0, 1.0, 1.0);"))

(defparameter *double-quad*
  (concatenate '(array single-float (*))
               (make-instance 'double :x -0.5 :y 0.5 :z 0.0) ; top-left
               (make-instance 'double :x 0.5 :y 0.5 :z 0.0) ; top-right
               (make-instance 'double :x -0.5 :y -0.5 :z 0.0) ; bottom-left
               (make-instance 'double :x 0.5 :y -0.5 :z 0.0))) ; bottom-right

(defun double ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :x 100
                           :y 100
                           :w 250
                           :h 250
                           :title "double")
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ((double
                                (:vertices *double-quad*)
                                (:indices '(0 1 2 2 3 1))
                                (:uniform transform)))
          (sdl2:with-event-loop (:method :poll)
            (:quit ()
              t)
            (:idle ()
              (fude-gl::with-clear (win (:color-buffer-bit)
                                        :color '(0.2 0.3 0.3 1.0))
                (gl:uniform-matrix transform 4
                                   (vector
                                     (3d-matrices:marr
                                       (3d-matrices:nmrotate
                                         (3d-matrices:meye 4)
                                         3d-vectors:+vz+
                                         (fude-gl:radians
                                           (get-internal-real-time))))))
                (fude-gl:draw-elements :triangles (fude-gl:indices-of
                                                   double))))))))))

;;;; Coordinate-system

(fude-gl:defshader coordinate-system 330 (fude-gl::xyz)
  (:vertex (&uniform (model :mat4) (view :mat4) (projection :mat4))
    "gl_Position = projection * view * model * vec4(xyz, 1.0);")
  (:fragment ((color :vec4)) "color = vec4(1.0, 1.0, 1.0, 1.0);"))

(defparameter *coord-quad*
  (concatenate '(array single-float (*))
               (make-instance 'coordinate-system :x -0.5 :y 0.5 :z 0.0) ; top-left
               (make-instance 'coordinate-system :x 0.5 :y 0.5 :z 0.0) ; top-right
               (make-instance 'coordinate-system :x -0.5 :y -0.5 :z 0.0) ; bottom-left
               (make-instance 'coordinate-system :x 0.5 :y -0.5 :z 0.0))) ; bottom-right

(defun demo ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :x 100
                           :y 100
                           :w 250
                           :h 250)
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ((coordinate-system
                                (:vertices *coord-quad*)
                                (:indices '(0 1 2 2 3 1))
                                (:uniform model view projection)))
          (sdl2:with-event-loop (:method :poll)
            (:quit ()
              t)
            (:idle ()
              (fude-gl::with-clear (win (:color-buffer-bit)
                                        :color '(0.2 0.3 0.3 1.0))
                (let ((m(3d-matrices:nmrotate
                               (3d-matrices:meye 4)
                               3d-vectors:+vx+
                               (fude-gl:radians -55)))
                      (v(3d-matrices:mtranslation
                              (3d-vectors:vec 0 0 -3)))
                      (p(3d-matrices:mperspective
                                    (fude-gl:radians 75)
                                    1 ; (/ w h)
                                    0.1
                                    100)))
                  (mapc(lambda (uniform matrix)
                            (gl:uniform-matrix uniform 4 (vector matrix)))
                          (list model view projection)
                          (mapcar #'3d-matrices:marr (list  m v p)))
                  (fude-gl:draw-elements :triangles (fude-gl:indices-of
                                                      coordinate-system)))))))))))


