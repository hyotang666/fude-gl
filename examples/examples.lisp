(defpackage :fude-gl-examples
  (:use :cl)
  (:export #:demos))

(in-package :fude-gl-examples)

;;;; INTERFACE

(defun demos ()
  (restart-case (mapc #'funcall
                      '(hello-triangle uniform-demo colored-triangle
                                       element-buffer texture-demo mix-demo
                                       hello double transform-demo translate-x
                                       translate-y scaling rotating coord-demo
                                       depth-demo cubes cameras walk-around))
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
        (fude-gl:with-shader ((hello-triangle (:vertices *triangle*)))
          (sdl2:with-event-loop (:method :poll)
            (:quit ()
              t)
            (:idle ()
              (fude-gl:with-clear (win (:color-buffer-bit))
                (gl:draw-arrays :triangles 0 3)))))))))

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
                                (:vertices *colored-triangle*)))
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
                                (:uniform (tex-loc tex))))
          (fude-gl:with-textures ((tex :texture-2d
                                       :init (fude-gl:tex-image-2d *png*)))
            (gl:uniformi tex-loc tex)
            (sdl2:with-event-loop (:method :poll)
              (:quit ()
                t)
              (:idle ()
                (fude-gl:with-clear (win (:color-buffer-bit))
                  (fude-gl:draw-elements :triangles (fude-gl:indices-of
                                                     texture-demo)))))))))))

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
                                (:uniform (tex1-loc tex1) (tex2-loc tex2))))
          (fude-gl:with-textures ((tex1 :texture-2d
                                        :init (fude-gl:tex-image-2d *png*))
                                  (tex2 :texture-2d
                                        :init (fude-gl:tex-image-2d *logo*)))
            (gl:uniformi tex1-loc tex1)
            (gl:uniformi tex2-loc tex2)
            (sdl2:with-event-loop (:method :poll)
              (:quit ()
                t)
              (:idle ()
                (sdl2:gl-swap-window win)
                (fude-gl:draw-elements :triangles (fude-gl:indices-of
                                                   mix-demo))))))))))

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
                                         (3d-matrices:meye 4) 3d-vectors:+vz+
                                         (fude-gl:radians
                                           (get-internal-real-time))))))
                (fude-gl:draw-elements :triangles (fude-gl:indices-of
                                                   double))))))))))

;;;; MATRIX-OPERATIONS

(let ((pathname (merge-pathnames "container.jpg" (user-homedir-pathname))))
  (unless (probe-file pathname)
    (dex:fetch "https://learnopengl.com/img/textures/container.jpg" pathname))
  (defparameter *image* (opticl:read-jpeg-file pathname)))

(let ((pathname (merge-pathnames "awesomeface.png" (user-homedir-pathname))))
  (unless (probe-file pathname)
    (dex:fetch "https://learnopengl.com/img/textures/awesomeface.png"
               pathname))
  (defparameter *face*
    (opticl:vertical-flip-image (opticl:read-png-file pathname))))

(fude-gl:defshader transform-demo 330 (fude-gl:xy fude-gl:st)
  (:vertex ((coord :vec2) &uniform (transform :mat4))
    "gl_Position = transform * vec4(xy, 0.0, 1.0);"
    "coord = st;")
  (:fragment ((color :vec4) &uniform (tex1 :|sampler2D|) (tex2 :|sampler2D|))
    "color = mix(texture(tex1, coord), texture(tex2, coord), 0.2);"))

(defparameter *texture-quad*
  (concatenate '(array single-float (*))
               (make-instance 'transform-demo :x -0.5 :y 0.5 :s 0.0 :t 1.0) ; top
                                                                            ; left
               (make-instance 'transform-demo :x 0.5 :y 0.5 :s 1.0 :t 1.0) ; top
                                                                           ; right
               (make-instance 'transform-demo :x -0.5 :y -0.5 :s 0.0 :t 0.0) ; bottom
                                                                             ; left
               (make-instance 'transform-demo :x 0.5 :y -0.5 :s 1.0 :t 0.0))) ; bottom right

(defun transform-demo ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :x 100
                           :y 100
                           :w 800
                           :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ((transform-demo
                                (:vertices *texture-quad*)
                                (:indices '(0 1 2 2 3 1))
                                (:uniform tex1 tex2 transform)))
          (fude-gl:with-textures ((image :texture-2d
                                         :init (fude-gl:tex-image-2d *image*))
                                  (face :texture-2d
                                        :init (fude-gl:tex-image-2d *face*)))
            (gl:uniformi tex1 image)
            (gl:uniformi tex2 face)
            (sdl2:with-event-loop (:method :poll)
              (:quit ()
                t)
              (:idle ()
                (fude-gl:with-clear (win (:color-buffer-bit))
                  (gl:uniform-matrix transform 4
                                     (vector
                                       (3d-matrices:marr
                                         (3d-matrices:nmscale
                                           (3d-matrices:nmrotate
                                             (3d-matrices:meye 4)
                                             3d-vectors:+vz+
                                             (fude-gl:radians 90))
                                           (3d-vectors:vec 0.5 0.5 0.5)))))
                  (fude-gl:draw-elements :triangles (fude-gl:indices-of
                                                     transform-demo)))))))))))

(defun translate-x ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :x 100
                           :y 100
                           :w 800
                           :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ((transform-demo
                                (:vertices *texture-quad*)
                                (:indices '(0 1 2 2 3 1))
                                (:uniform tex1 tex2 transform)))
          (fude-gl:with-textures ((image :texture-2d
                                         :init (fude-gl:tex-image-2d *image*))
                                  (face :texture-2d
                                        :init (fude-gl:tex-image-2d *face*)))
            (gl:uniformi tex1 image)
            (gl:uniformi tex2 face)
            (sdl2:with-event-loop (:method :poll)
              (:quit ()
                t)
              (:idle ()
                (fude-gl:with-clear (win (:color-buffer-bit))
                  (sleep (/ 1 60))
                  (gl:uniform-matrix transform 4
                                     (vector
                                       (3d-matrices:marr
                                         (3d-matrices:mtranslation
                                           (3d-vectors:vec
                                             (sin (get-internal-real-time)) 0.0
                                             0.0)))))
                  (fude-gl:draw-elements :triangles (fude-gl:indices-of
                                                     transform-demo)))))))))))

(defun translate-y ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :x 100
                           :y 100
                           :w 800
                           :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ((transform-demo
                                (:vertices *texture-quad*)
                                (:indices '(0 1 2 2 3 1))
                                (:uniform tex1 tex2 transform)))
          (fude-gl:with-textures ((image :texture-2d
                                         :init (fude-gl:tex-image-2d *image*))
                                  (face :texture-2d
                                        :init (fude-gl:tex-image-2d *face*)))
            (gl:uniformi tex1 image)
            (gl:uniformi tex2 face)
            (sdl2:with-event-loop (:method :poll)
              (:quit ()
                t)
              (:idle ()
                (fude-gl:with-clear (win (:color-buffer-bit))
                  (sleep (/ 1 60))
                  (gl:uniform-matrix transform 4
                                     (vector
                                       (3d-matrices:marr
                                         (3d-matrices:mtranslation
                                           (3d-vectors:vec 0.0
                                                           (sin
                                                             (get-internal-real-time))
                                                           0.0)))))
                  (fude-gl:draw-elements :triangles (fude-gl:indices-of
                                                     transform-demo)))))))))))

(defun scaling ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :x 100
                           :y 100
                           :w 800
                           :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ((transform-demo
                                (:vertices *texture-quad*)
                                (:indices '(0 1 2 2 3 1))
                                (:uniform tex1 tex2 transform)))
          (fude-gl:with-textures ((image :texture-2d
                                         :init (fude-gl:tex-image-2d *image*))
                                  (face :texture-2d
                                        :init (fude-gl:tex-image-2d *face*)))
            (gl:uniformi tex1 image)
            (gl:uniformi tex2 face)
            (sdl2:with-event-loop (:method :poll)
              (:quit ()
                t)
              (:idle ()
                (fude-gl:with-clear (win (:color-buffer-bit))
                  (sleep (/ 1 5))
                  (gl:uniform-matrix transform 4
                                     (vector
                                       (3d-matrices:marr
                                         (3d-matrices:nmscale
                                           (3d-matrices:mtranslation
                                             (3d-vectors:vec 0 0 0))
                                           (let ((v
                                                  (abs
                                                    (sin
                                                      (get-internal-real-time)))))
                                             (3d-vectors:vec v v 0.0))))))
                  (fude-gl:draw-elements :triangles (fude-gl:indices-of
                                                     transform-demo)))))))))))

(defun rotating ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :x 100
                           :y 100
                           :w 800
                           :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ((transform-demo
                                (:vertices *texture-quad*)
                                (:indices '(0 1 2 2 3 1))
                                (:uniform tex1 tex2 transform)))
          (fude-gl:with-textures ((image :texture-2d
                                         :init (fude-gl:tex-image-2d *image*))
                                  (face :texture-2d
                                        :init (fude-gl:tex-image-2d *face*)))
            (gl:uniformi tex1 image)
            (gl:uniformi tex2 face)
            (sdl2:with-event-loop (:method :poll)
              (:quit ()
                t)
              (:idle ()
                (fude-gl:with-clear (win (:color-buffer-bit))
                  (sleep (/ 1 10))
                  (gl:uniform-matrix transform 4
                                     (vector
                                       (3d-matrices:marr
                                         (3d-matrices:nmrotate
                                           (3d-matrices:mtranslation
                                             (3d-vectors:vec 0 0 0))
                                           3d-vectors:+vz+
                                           (fude-gl:radians
                                             (get-internal-real-time))))))
                  (fude-gl:draw-elements :triangles (fude-gl:indices-of
                                                     transform-demo)))))))))))

;;;; COORD-DEMO

(fude-gl:defshader coord-demo 330 (fude-gl:xy fude-gl:st)
  (:vertex ((coord :vec2) &uniform (model :mat4) (view :mat4)
            (projection :mat4))
    "gl_Position = projection * view * model * vec4(xy, 0.0, 1.0);"
    "coord = st;")
  (:fragment ((color :vec4) &uniform (tex1 :|sampler2D|) (tex2 :|sampler2D|))
    "color = mix(texture(tex1, coord), texture(tex2, coord), 0.2);"))

(defparameter *texture-quad*
  (concatenate '(array single-float (*))
               (make-instance 'coord-demo :x -0.5 :y 0.5 :s 0.0 :t 1.0) ; top
                                                                        ; left
               (make-instance 'coord-demo :x 0.5 :y 0.5 :s 1.0 :t 1.0) ; top
                                                                       ; right
               (make-instance 'coord-demo :x -0.5 :y -0.5 :s 0.0 :t 0.0) ; bottom
                                                                         ; left
               (make-instance 'coord-demo :x 0.5 :y -0.5 :s 1.0 :t 0.0))) ; bottom right

(defun coord-demo ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :x 100
                           :y 100
                           :w 800
                           :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ((coord-demo
                                (:vertices *texture-quad*)
                                (:indices '(0 1 2 2 3 1))
                                (:uniform tex1 tex2 model view projection)))
          (fude-gl:with-textures ((image :texture-2d
                                         :init (fude-gl:tex-image-2d *image*))
                                  (face :texture-2d
                                        :init (fude-gl:tex-image-2d *face*)))
            (gl:uniformi tex1 image)
            (gl:uniformi tex2 face)
            (let ((m
                   (3d-matrices:nmrotate (3d-matrices:meye 4) 3d-vectors:+vx+
                                         (fude-gl:radians -55)))
                  (v (3d-matrices:mtranslation (3d-vectors:vec 0 0 -3)))
                  (p
                   (3d-matrices:mperspective 45
                                             (multiple-value-call #'/
                                               (sdl2:get-window-size win))
                                             0.1 100)))
              (flet ((send (matrix uniform)
                       (gl:uniform-matrix uniform 4
                                          (vector (3d-matrices:marr matrix)))))
                (sdl2:with-event-loop (:method :poll)
                  (:quit ()
                    t)
                  (:idle ()
                    (fude-gl:with-clear (win (:color-buffer-bit))
                      (send m model)
                      (send v view)
                      (send p projection)
                      (fude-gl:draw-elements :triangles (fude-gl:indices-of
                                                         coord-demo)))))))))))))

;;;; DEPTH-DEMO

(fude-gl:defshader depth-demo 330 (fude-gl:xyz fude-gl:st)
  (:vertex ((coord :vec2) &uniform (model :mat4) (view :mat4)
            (projection :mat4))
    "gl_Position = projection * view * model * vec4(xyz, 1.0);"
    "coord = st;")
  (:fragment ((color :vec4) &uniform (tex1 :|sampler2D|) (tex2 :|sampler2D|))
    "color = mix(texture(tex1, coord), texture(tex2, coord), 0.2);"))

(defparameter *depth-demo*
  (flet ((make (x y z s %t)
           (make-instance 'depth-demo :x x :y y :z z :s s :t %t)))
    (concatenate '(array single-float (*))
                 ;;
                 (make -0.5 -0.5 -0.5 0.0 0.0) (make 0.5 -0.5 -0.5 1.0 0.0)
                 (make 0.5 0.5 -0.5 1.0 1.0) (make 0.5 0.5 -0.5 1.0 1.0)
                 (make -0.5 0.5 -0.5 0.0 1.0) (make -0.5 -0.5 -0.5 0.0 0.0)
                 ;;
                 (make -0.5 -0.5 0.5 0.0 0.0) (make 0.5 -0.5 0.5 1.0 0.0)
                 (make 0.5 0.5 0.5 1.0 1.0) (make 0.5 0.5 0.5 1.0 1.0)
                 (make -0.5 0.5 0.5 0.0 1.0) (make -0.5 -0.5 0.5 0.0 0.0)
                 ;;
                 (make -0.5 0.5 0.5 1.0 0.0) (make -0.5 0.5 -0.5 1.0 1.0)
                 (make -0.5 -0.5 -0.5 0.0 1.0) (make -0.5 -0.5 -0.5 0.0 1.0)
                 (make -0.5 -0.5 0.5 0.0 0.0) (make -0.5 0.5 0.5 1.0 0.0)
                 ;;
                 (make 0.5 0.5 0.5 1.0 0.0) (make 0.5 0.5 -0.5 1.0 1.0)
                 (make 0.5 -0.5 -0.5 0.0 1.0) (make 0.5 -0.5 -0.5 0.0 1.0)
                 (make 0.5 -0.5 0.5 0.0 0.0) (make 0.5 0.5 0.5 1.0 0.0)
                 ;;
                 (make -0.5 -0.5 -0.5 0.0 1.0) (make 0.5 -0.5 -0.5 1.0 1.0)
                 (make 0.5 -0.5 0.5 1.0 0.0) (make 0.5 -0.5 0.5 1.0 0.0)
                 (make -0.5 -0.5 0.5 0.0 0.0) (make -0.5 -0.5 -0.5 0.0 1.0)
                 ;;
                 (make -0.5 0.5 -0.5 0.0 1.0) (make 0.5 0.5 -0.5 1.0 1.0)
                 (make 0.5 0.5 0.5 1.0 0.0) (make 0.5 0.5 0.5 1.0 0.0)
                 (make -0.5 0.5 0.5 0.0 0.0) (make -0.5 0.5 -0.5 0.0 1.0))))

(defun depth-demo ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :x 100
                           :y 100
                           :w 800
                           :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ((depth-demo
                                (:vertices *depth-demo*)
                                (:uniform tex1 tex2 model view projection)))
          (fude-gl:with-textures ((image :texture-2d
                                         :init (fude-gl:tex-image-2d *image*))
                                  (face :texture-2d
                                        :init (fude-gl:tex-image-2d *face*)))
            (gl:uniformi tex1 image)
            (gl:uniformi tex2 face)
            (flet ((send (matrix uniform)
                     (gl:uniform-matrix uniform 4
                                        (vector (3d-matrices:marr matrix)))))
              (gl:enable :depth-test)
              (sdl2:with-event-loop (:method :poll)
                (:quit ()
                  t)
                (:idle ()
                  (sleep (/ 1 15))
                  (fude-gl:with-clear (win (:color-buffer-bit :depth-buffer-bit))
                    (let ((m
                           (3d-matrices:nmrotate (3d-matrices:meye 4)
                                                 (3d-vectors:vec 0.5 1 0)
                                                 (fude-gl:radians
                                                   (get-internal-real-time))))
                          (v
                           (3d-matrices:mtranslation (3d-vectors:vec 0 0 -3)))
                          (p
                           (3d-matrices:mperspective 45
                                                     (multiple-value-call #'/
                                                       (sdl2:get-window-size
                                                         win))
                                                     0.1 100)))
                      (send m model)
                      (send v view)
                      (send p projection)
                      (gl:draw-arrays :triangles 0 36))))))))))))

;;;; CUBES

(fude-gl:defshader cubes 330 (fude-gl:xyz fude-gl:st)
  (:vertex ((coord :vec2) &uniform (model :mat4) (view :mat4)
            (projection :mat4))
    "gl_Position = projection * view * model * vec4(xyz, 1.0);"
    "coord = st;")
  (:fragment ((color :vec4) &uniform (tex1 :|sampler2D|) (tex2 :|sampler2D|))
    "color = mix(texture(tex1, coord), texture(tex2, coord), 0.2);"))

(defun cubes ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :x 100
                           :y 100
                           :w 800
                           :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ((cubes
                                (:vertices *depth-demo*)
                                (:uniform tex1 tex2 model view projection)))
          (fude-gl:with-textures ((image :texture-2d
                                         :init (fude-gl:tex-image-2d *image*))
                                  (face :texture-2d
                                        :init (fude-gl:tex-image-2d *face*)))
            (gl:uniformi tex1 image)
            (gl:uniformi tex2 face)
            (flet ((send (matrix uniform)
                     (gl:uniform-matrix uniform 4
                                        (vector (3d-matrices:marr matrix)))))
              (let ((cube-positions
                     (list (3d-vectors:vec 0 0 0) (3d-vectors:vec 2 5 -15)
                           (3d-vectors:vec -1.5 -2.2 -2.5)
                           (3d-vectors:vec -3.8 -2.0 -12.3)
                           (3d-vectors:vec 2.4 -0.4 -3.5)
                           (3d-vectors:vec -1.7 3 -7.5)
                           (3d-vectors:vec 1.3 -2 -2.5)
                           (3d-vectors:vec 1.5 2 -2.5)
                           (3d-vectors:vec 1.5 0.2 -1.5)
                           (3d-vectors:vec -1.3 1 -1.5))))
                (gl:enable :depth-test)
                (sdl2:with-event-loop (:method :poll)
                  (:quit ()
                    t)
                  (:idle ()
                    (sleep (/ 1 5))
                    (fude-gl:with-clear (win (:color-buffer-bit :depth-buffer-bit))
                      (loop :for pos :in cube-positions
                            :for i :upfrom 0
                            :do (let ((m
                                       (3d-matrices:nmrotate
                                         (3d-matrices:mtranslation pos)
                                         (3d-vectors:vec 1 0.3 0.5)
                                         (fude-gl:radians (* 20 i))))
                                      (v
                                       (3d-matrices:mtranslation
                                         (3d-vectors:vec 0 0 -3)))
                                      (p
                                       (3d-matrices:mperspective 45
                                                                 (multiple-value-call
                                                                     #'/
                                                                   (sdl2:get-window-size
                                                                     win))
                                                                 0.1 100)))
                                  (send m model)
                                  (send v view)
                                  (send p projection)
                                  (gl:draw-arrays :triangles 0 36))))))))))))))

;;;; CAMERAS

(defun cameras ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :x 100
                           :y 100
                           :w 800
                           :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ((cubes
                                (:vertices *depth-demo*)
                                (:uniform tex1 tex2 model view projection)))
          (fude-gl:with-textures ((image :texture-2d
                                         :init (fude-gl:tex-image-2d *image*))
                                  (face :texture-2d
                                        :init (fude-gl:tex-image-2d *face*)))
            (gl:uniformi tex1 image)
            (gl:uniformi tex2 face)
            (flet ((send (matrix uniform)
                     (gl:uniform-matrix uniform 4
                                        (vector (3d-matrices:marr matrix)))))
              (let ((cube-positions
                     (list (3d-vectors:vec 0 0 0) (3d-vectors:vec 2 5 -15)
                           (3d-vectors:vec -1.5 -2.2 -2.5)
                           (3d-vectors:vec -3.8 -2.0 -12.3)
                           (3d-vectors:vec 2.4 -0.4 -3.5)
                           (3d-vectors:vec -1.7 3 -7.5)
                           (3d-vectors:vec 1.3 -2 -2.5)
                           (3d-vectors:vec 1.5 2 -2.5)
                           (3d-vectors:vec 1.5 0.2 -1.5)
                           (3d-vectors:vec -1.3 1 -1.5))))
                (gl:enable :depth-test)
                (sdl2:with-event-loop (:method :poll)
                  (:quit ()
                    t)
                  (:idle ()
                    (sleep (/ 1 15))
                    (fude-gl:with-clear (win (:color-buffer-bit :depth-buffer-bit))
                      (let* ((radius 10)
                             (v
                              (3d-matrices:mlookat
                                (3d-vectors:vec
                                  (* (sin (get-internal-real-time)) radius) 0
                                  (* (cos (get-internal-real-time)) radius))
                                (3d-vectors:vec 0 0 0) (3d-vectors:vec 0 1 0))))
                        (loop :for pos :in cube-positions
                              :for i :upfrom 0
                              :do (let ((m
                                         (3d-matrices:nmrotate
                                           (3d-matrices:mtranslation pos)
                                           (3d-vectors:vec 1 0.3 0.5)
                                           (fude-gl:radians (* 20 i))))
                                        (p
                                         (3d-matrices:mperspective 45
                                                                   (multiple-value-call
                                                                       #'/
                                                                     (sdl2:get-window-size
                                                                       win))
                                                                   0.1 100)))
                                    (send m model)
                                    (send v view)
                                    (send p projection)
                                    (gl:draw-arrays :triangles 0
                                                    36)))))))))))))))

;;;; WALK-AROUND

(defparameter *cube-positions*
  (list (3d-vectors:vec 0 0 0) (3d-vectors:vec 2 5 -15)
        (3d-vectors:vec -1.5 -2.2 -2.5) (3d-vectors:vec -3.8 -2.0 -12.3)
        (3d-vectors:vec 2.4 -0.4 -3.5) (3d-vectors:vec -1.7 3 -7.5)
        (3d-vectors:vec 1.3 -2 -2.5) (3d-vectors:vec 1.5 2 -2.5)
        (3d-vectors:vec 1.5 0.2 -1.5) (3d-vectors:vec -1.3 1 -1.5)))

(defun move-camera (keysym camera-front camera-up camera-pos)
  (let ((camera-speed 0.05))
    (case (sdl2:scancode keysym)
      (:scancode-up
       (3d-vectors:nv+ camera-pos (3d-vectors:v* camera-speed camera-front)))
      (:scancode-down
       (3d-vectors:nv- camera-pos (3d-vectors:v* camera-speed camera-front)))
      (:scancode-left
       (3d-vectors:v- camera-pos
                      (3d-vectors:v*
                        (3d-vectors:vunit
                          (3d-vectors:vc camera-front camera-up))
                        camera-speed)))
      (:scancode-right
       (3d-vectors:v+ camera-pos
                      (3d-vectors:v*
                        (3d-vectors:vunit
                          (3d-vectors:vc camera-front camera-up))
                        camera-speed)))
      (otherwise camera-pos))))

(defun walk-around ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :x 100
                           :y 100
                           :w 800
                           :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ((cubes
                                (:vertices *depth-demo*)
                                (:uniform tex1 tex2 model view projection)))
          (fude-gl:with-textures ((image :texture-2d
                                         :init (fude-gl:tex-image-2d *image*))
                                  (face :texture-2d
                                        :init (fude-gl:tex-image-2d *face*)))
            (gl:uniformi tex1 image)
            (gl:uniformi tex2 face)
            (flet ((send (matrix uniform)
                     (gl:uniform-matrix uniform 4
                                        (vector (3d-matrices:marr matrix)))))
              (let ((camera-pos (3d-vectors:vec 0 0 3))
                    (camera-front (3d-vectors:vec 0 0 -1))
                    (camera-up (3d-vectors:vec 0 1 0))
                    (p
                     (3d-matrices:mperspective 45
                                               (multiple-value-call #'/
                                                 (sdl2:get-window-size win))
                                               0.1 100)))
                (gl:enable :depth-test)
                (sdl2:with-event-loop (:method :poll)
                  (:quit ()
                    t)
                  (:keydown (:keysym keysym)
                    (setf camera-pos
                            (move-camera keysym camera-front camera-up
                                         camera-pos)))
                  (:idle ()
                    (fude-gl:with-clear (win (:color-buffer-bit :depth-buffer-bit))
                      (loop :for pos :in *cube-positions*
                            :for i :upfrom 0
                            :for m
                                 = (3d-matrices:nmrotate
                                     (3d-matrices:mtranslation pos)
                                     (3d-vectors:vec 1 0.3 0.5)
                                     (fude-gl:radians (* 20 i)))
                            :do (send m model)
                                (send
                                  (3d-matrices:mlookat camera-pos
                                                       (3d-vectors:v+
                                                         camera-pos
                                                         camera-front)
                                                       camera-up)
                                  view)
                                (send p projection)
                                (gl:draw-arrays :triangles 0 36)))))))))))))

;;;; MOVEMENT-SPEED

(defparameter *delta-time* 0)

(defparameter *last-frame* 0)

(defun move-camera-with-delta (keysym camera-front camera-up camera-pos)
  (let ((camera-speed (* 0.05 (min 1 *delta-time*))))
    (case (sdl2:scancode keysym)
      (:scancode-up
       (3d-vectors:nv+ camera-pos (3d-vectors:v* camera-speed camera-front)))
      (:scancode-down
       (3d-vectors:nv- camera-pos (3d-vectors:v* camera-speed camera-front)))
      (:scancode-left
       (3d-vectors:v- camera-pos
                      (3d-vectors:v*
                        (3d-vectors:vunit
                          (3d-vectors:vc camera-front camera-up))
                        camera-speed)))
      (:scancode-right
       (3d-vectors:v+ camera-pos
                      (3d-vectors:v*
                        (3d-vectors:vunit
                          (3d-vectors:vc camera-front camera-up))
                        camera-speed)))
      (otherwise camera-pos))))

(defun movement-speed ()
  (uiop:nest (sdl2:with-init (:everything))
             (sdl2:with-window (win :flags '(:shown :opengl)
                                    :x 100
                                    :y 100
                                    :w 800
                                    :h 600))
             (sdl2:with-gl-context (context win))
             (fude-gl:with-shader ((cubes
                                     (:vertices *depth-demo*)
                                     (:uniform tex1 tex2 model view
                                               projection))))
             (fude-gl:with-textures ((image :texture-2d
                                            :init (fude-gl:tex-image-2d
                                                    *image*))
                                     (face :texture-2d
                                           :init (fude-gl:tex-image-2d
                                                   *face*))))
             (flet ((send (matrix uniform)
                          (gl:uniform-matrix uniform 4
                                             (vector
                                               (3d-matrices:marr matrix))))))
             (let ((camera-pos (3d-vectors:vec 0 0 3))
                   (camera-front (3d-vectors:vec 0 0 -1))
                   (camera-up (3d-vectors:vec 0 1 0))
                   (p
                    (3d-matrices:mperspective 45
                                              (multiple-value-call #'/
                                                (sdl2:get-window-size win))
                                              0.1 100))))
             (sdl2:with-event-loop (:method :poll)
               (:initialize ()
                 (gl:uniformi tex1 image)
                 (gl:uniformi tex2 face)
                 (gl:enable :depth-test))
               (:quit ()
                 t)
               (:keydown (:keysym keysym)
                 (setf camera-pos
                         (move-camera-with-delta keysym camera-front camera-up
                                                 camera-pos)))
               (:idle ()
                 (let ((current-frame (sdl2:get-ticks)))
                   (setf *delta-time* (- current-frame *last-frame*)
                         *last-frame* current-frame))
                 (fude-gl:with-clear (win (:color-buffer-bit :depth-buffer-bit))
                   (loop :for pos :in *cube-positions*
                         :for i :upfrom 0
                         :for m
                              = (3d-matrices:nmrotate
                                  (3d-matrices:mtranslation pos)
                                  (3d-vectors:vec 1 0.3 0.5)
                                  (fude-gl:radians (* 20 i)))
                         :do (send m model)
                             (send
                              (3d-matrices:mlookat camera-pos
                                                   (3d-vectors:v+ camera-pos
                                                                  camera-front)
                                                   camera-up)
                              view)
                             (send p projection)
                             (gl:draw-arrays :triangles 0 36)))))))
