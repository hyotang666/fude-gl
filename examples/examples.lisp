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
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :x 100
                           :y 100
                           :w 800
                           :h 600))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader ((hello-triangle (:vertices nil *triangle*))))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      (gl:draw-arrays :triangles 0 3))))

;;;; UNIFORM-DEMO

(fude-gl:defshader uniform-demo 330 (fude-gl:xy)
  ;; Re-use vertex-shader of HELLO-TRIANGLE.
  (:vertex () 'hello-triangle)
  (:fragment ((|outColor| :vec4) &uniform (|triangleColor| :vec3))
    "outColor = vec4(triangleColor, 1.0);"))

(defun uniform-demo ()
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :x 100
                           :y 100
                           :w 800
                           :h 600))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader ((uniform-demo
                            (:vertices nil *triangle*)
                            (:uniform |triangleColor|))))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t)
      (:idle ()
        (fude-gl:with-clear (win (:color-buffer-bit))
          (fude-gl::in-shader uniform-demo)
          (gl:uniformf |triangleColor|
                       (/ (+ 1.0 (sin (get-internal-real-time))) 2) 0.0 0.0)
          (gl:draw-arrays :triangles 0 3))))))

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
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :x 100
                           :y 100
                           :w 800
                           :h 600))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader ((colored-triangle
                            (:vertices nil *colored-triangle*)))
      (fude-gl::in-shader colored-triangle)
      (sdl2:with-event-loop (:method :poll)
        (:quit ()
          t)
        (:idle ()
          (fude-gl:with-clear (win (:color-buffer-bit))
            (gl:draw-arrays :triangles 0 3)))))))

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
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :x 100
                           :y 100
                           :w 800
                           :h 600))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader ((colored-triangle
                            (:vertices nil *element-buffer-example*)
                            (:indices '(0 1 2 2 3 0))))
      (fude-gl::in-shader colored-triangle))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      (fude-gl:draw-elements :triangles (fude-gl:indices-of
                                         colored-triangle)))))

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
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :x 100
                           :y 100
                           :w 800
                           :h 600))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader ((texture-demo
                            (:vertices nil *quad*)
                            (:indices '(0 1 2 2 3 0))
                            (:uniform (tex-loc tex)))))
    (fude-gl:with-textures ((tex :texture-2d
                                 :init (fude-gl:tex-image-2d *png*)))
      (fude-gl::in-shader texture-demo)
      (gl:uniformi tex-loc (fude-gl::texture-id tex)))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      (fude-gl:draw-elements :triangles (fude-gl:indices-of texture-demo)))))

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
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :x 100
                           :y 100
                           :w 800
                           :h 600))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader ((mix-demo
                            (:vertices nil *mix-demo*)
                            (:indices '(0 1 2 2 3 0))
                            (:uniform (tex1-loc tex1) (tex2-loc tex2)))))
    (fude-gl:with-textures ((tex1 :texture-2d
                                  :init (fude-gl:tex-image-2d *png*))
                            (tex2 :texture-2d
                                  :init (fude-gl:tex-image-2d *logo*)))
      (fude-gl::in-shader mix-demo)
      (gl:uniformi tex1-loc (fude-gl::texture-id tex1))
      (gl:uniformi tex2-loc (fude-gl::texture-id tex2)))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil (sdl2:gl-swap-window win)
     (fude-gl:draw-elements :triangles (fude-gl:indices-of mix-demo)))))

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
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :x 100
                           :y 100
                           :w 250
                           :h 250
                           :title "hello"))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader ((hello
                            (:vertices nil *hello-quad*)
                            (:indices '(0 1 2 2 3 1))))
      (fude-gl::in-shader hello))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      (fude-gl:draw-elements :triangles (fude-gl:indices-of hello)))))

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
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :x 100
                           :y 100
                           :w 250
                           :h 250
                           :title "double"))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader ((double
                            (:vertices nil *double-quad*)
                            (:indices '(0 1 2 2 3 1))
                            (:uniform transform)))
      (fude-gl::in-shader double))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl::with-clear (win (:color-buffer-bit) :color '(0.2 0.3 0.3 1.0))
      (gl:uniform-matrix transform 4
                         (vector
                           (3d-matrices:marr
                             (3d-matrices:nmrotate (3d-matrices:meye 4)
                                                   3d-vectors:+vz+
                                                   (fude-gl:radians
                                                     (get-internal-real-time))))))
      (fude-gl:draw-elements :triangles (fude-gl:indices-of double)))))

;;;; MATRIX-OPERATIONS

(defparameter *image*
  (let ((pathname (merge-pathnames "container.jpg" (user-homedir-pathname))))
    (unless (probe-file pathname)
      (dex:fetch "https://learnopengl.com/img/textures/container.jpg"
                 pathname))
    (opticl:read-jpeg-file pathname)))

(defparameter *face*
  (let ((pathname (merge-pathnames "awesomeface.png" (user-homedir-pathname))))
    (unless (probe-file pathname)
      (dex:fetch "https://learnopengl.com/img/textures/awesomeface.png"
                 pathname))
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
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :x 100
                           :y 100
                           :w 800
                           :h 600))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader ((transform-demo
                            (:vertices nil *texture-quad*)
                            (:indices '(0 1 2 2 3 1))
                            (:uniform tex1 tex2 transform))))
    (fude-gl:with-textures ((image :texture-2d
                                   :init (fude-gl:tex-image-2d *image*))
                            (face :texture-2d
                                  :init (fude-gl:tex-image-2d *face*)))
      (fude-gl::in-shader transform-demo)
      (gl:uniformi tex1 (fude-gl::texture-id image))
      (gl:uniformi tex2 (fude-gl::texture-id face)))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      (gl:uniform-matrix transform 4
                         (vector
                           (3d-matrices:marr
                             (3d-matrices:nmscale
                               (3d-matrices:nmrotate (3d-matrices:meye 4)
                                                     3d-vectors:+vz+
                                                     (fude-gl:radians 90))
                               (3d-vectors:vec 0.5 0.5 0.5)))))
      (fude-gl:draw-elements :triangles (fude-gl:indices-of transform-demo)))))

(defun translate-x ()
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :x 100
                           :y 100
                           :w 800
                           :h 600))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader ((transform-demo
                            (:vertices nil *texture-quad*)
                            (:indices '(0 1 2 2 3 1))
                            (:uniform tex1 tex2 transform))))
    (fude-gl:with-textures ((image :texture-2d
                                   :init (fude-gl:tex-image-2d *image*))
                            (face :texture-2d
                                  :init (fude-gl:tex-image-2d *face*)))
      (fude-gl::in-shader transform-demo)
      (gl:uniformi tex1 (fude-gl::texture-id image))
      (gl:uniformi tex2 (fude-gl::texture-id face)))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      (gl:uniform-matrix transform 4
                         (vector
                           (3d-matrices:marr
                             (3d-matrices:mtranslation
                               (3d-vectors:vec (sin (get-internal-real-time))
                                               0.0 0.0)))))
      (fude-gl:draw-elements :triangles (fude-gl:indices-of transform-demo)))))

(defun translate-y ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :x 100
                           :y 100
                           :w 800
                           :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ((transform-demo
                                (:vertices nil *texture-quad*)
                                (:indices '(0 1 2 2 3 1))
                                (:uniform tex1 tex2 transform)))
          (fude-gl:with-textures ((image :texture-2d
                                         :init (fude-gl:tex-image-2d *image*))
                                  (face :texture-2d
                                        :init (fude-gl:tex-image-2d *face*)))
            (fude-gl::in-shader transform-demo)
            (gl:uniformi tex1 (fude-gl::texture-id image))
            (gl:uniformi tex2 (fude-gl::texture-id face))
            (sdl2:with-event-loop (:method :poll)
              (:quit ()
                t)
              (:idle ()
                (fude-gl:with-clear (win (:color-buffer-bit))
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
                                (:vertices nil *texture-quad*)
                                (:indices '(0 1 2 2 3 1))
                                (:uniform tex1 tex2 transform)))
          (fude-gl:with-textures ((image :texture-2d
                                         :init (fude-gl:tex-image-2d *image*))
                                  (face :texture-2d
                                        :init (fude-gl:tex-image-2d *face*)))
            (fude-gl::in-shader transform-demo)
            (gl:uniformi tex1 (fude-gl::texture-id image))
            (gl:uniformi tex2 (fude-gl::texture-id face))
            (sdl2:with-event-loop (:method :poll)
              (:quit ()
                t)
              (:idle ()
                (fude-gl:with-clear (win (:color-buffer-bit))
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
                                (:vertices nil *texture-quad*)
                                (:indices '(0 1 2 2 3 1))
                                (:uniform tex1 tex2 transform)))
          (fude-gl:with-textures ((image :texture-2d
                                         :init (fude-gl:tex-image-2d *image*))
                                  (face :texture-2d
                                        :init (fude-gl:tex-image-2d *face*)))
            (fude-gl::in-shader transform-demo)
            (gl:uniformi tex1 (fude-gl::texture-id image))
            (gl:uniformi tex2 (fude-gl::texture-id face))
            (sdl2:with-event-loop (:method :poll)
              (:quit ()
                t)
              (:idle ()
                (fude-gl:with-clear (win (:color-buffer-bit))
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
                                (:vertices nil *texture-quad*)
                                (:indices '(0 1 2 2 3 1))
                                (:uniform tex1 tex2 model view projection)))
          (fude-gl:with-textures ((image :texture-2d
                                         :init (fude-gl:tex-image-2d *image*))
                                  (face :texture-2d
                                        :init (fude-gl:tex-image-2d *face*)))
            (fude-gl::in-shader coord-demo)
            (gl:uniformi tex1 (fude-gl::texture-id image))
            (gl:uniformi tex2 (fude-gl::texture-id face))
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
                                (:vertices nil *depth-demo*)
                                (:uniform tex1 tex2 model view projection)))
          (fude-gl:with-textures ((image :texture-2d
                                         :init (fude-gl:tex-image-2d *image*))
                                  (face :texture-2d
                                        :init (fude-gl:tex-image-2d *face*)))
            (fude-gl::in-shader depth-demo)
            (gl:uniformi tex1 (fude-gl::texture-id image))
            (gl:uniformi tex2 (fude-gl::texture-id face))
            (flet ((send (matrix uniform)
                     (gl:uniform-matrix uniform 4
                                        (vector (3d-matrices:marr matrix)))))
              (gl:enable :depth-test)
              (sdl2:with-event-loop (:method :poll)
                (:quit ()
                  t)
                (:idle ()
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
                                (:vertices nil *depth-demo*)
                                (:uniform tex1 tex2 model view projection)))
          (fude-gl:with-textures ((image :texture-2d
                                         :init (fude-gl:tex-image-2d *image*))
                                  (face :texture-2d
                                        :init (fude-gl:tex-image-2d *face*)))
            (fude-gl::in-shader cubes)
            (gl:uniformi tex1 (fude-gl::texture-id image))
            (gl:uniformi tex2 (fude-gl::texture-id face))
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
                                (:vertices nil *depth-demo*)
                                (:uniform tex1 tex2 model view projection)))
          (fude-gl:with-textures ((image :texture-2d
                                         :init (fude-gl:tex-image-2d *image*))
                                  (face :texture-2d
                                        :init (fude-gl:tex-image-2d *face*)))
            (fude-gl::in-shader cubes)
            (gl:uniformi tex1 (fude-gl::texture-id image))
            (gl:uniformi tex2 (fude-gl::texture-id face))
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
                                (:vertices nil *depth-demo*)
                                (:uniform tex1 tex2 model view projection)))
          (fude-gl:with-textures ((image :texture-2d
                                         :init (fude-gl:tex-image-2d *image*))
                                  (face :texture-2d
                                        :init (fude-gl:tex-image-2d *face*)))
            (fude-gl::in-shader cubes)
            (gl:uniformi tex1 (fude-gl::texture-id image))
            (gl:uniformi tex2 (fude-gl::texture-id face))
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
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :x 100
                           :y 100
                           :w 800
                           :h 600))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader ((cubes
                            (:vertices nil *depth-demo*)
                            (:uniform tex1 tex2 model view projection))))
    (fude-gl:with-textures ((image :texture-2d
                                   :init (fude-gl:tex-image-2d *image*))
                            (face :texture-2d
                                  :init (fude-gl:tex-image-2d *face*))))
    (flet ((send (matrix uniform)
                 (gl:uniform-matrix uniform 4
                                    (vector (3d-matrices:marr matrix))))))
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
                     = (3d-matrices:nmrotate (3d-matrices:mtranslation pos)
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

;;;; FONT

(defun text ()
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl) :w 800 :h 600))
    (sdl2:with-gl-context (context win)
      (gl:enable :blend)
      (gl:blend-func :src-alpha :one-minus-src-alpha))
    (fude-gl::with-text-renderer (renderer :size 32 :win win))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t)
      (:idle ()
        (fude-gl:with-clear (win (:color-buffer-bit))
          (renderer "Hello world! g" :x 0 :y :center))))))

;;;; INSTANCING

(defparameter *instancing*
  (coerce
    #(-0.05 0.05 1.0 0.0 0.0 ; first
      0.05 -0.05 0.0 1.0 0.0 ; second
      -0.05 -0.05 0.0 0.0 1.0 ; third
      -0.05 0.05 1.0 0.0 0.0 ; fourth
      0.05 -0.05 0.0 1.0 0.0 ; fifth
      0.05 0.05 0.0 1.0 1.0 ; sixth
      )
    '(array single-float (*))))

(fude-gl:defshader instancing 330 (fude-gl:xy fude-gl:rgb)
  (:vertex ((|fColor| :vec3) &uniform (offsets :vec2 100))
    "vec2 offset = offsets[gl_InstanceID];"
    "gl_Position = vec4(xy + offset, 0.0, 1.0);"
    "fColor = rgb;")
  (:fragment ((|fragColor| :vec4)) "fragColor = vec4(fColor, 1.0);"))

(defun instancing ()
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl) :w 800 :h 600))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader ((instancing
                            (:vertices nil *instancing*)
                            (:uniform offsets)
                            (:vertex-array vao))))
    (let ((translations
           (uiop:while-collecting (acc)
             (loop :with offset = 0.1
                   :for y :upfrom -10 :below 10 :by 2
                   :do (loop :for x :upfrom -10 :below 10 :by 2
                             :do (acc
                                  (vector (+ (/ x 10) offset)
                                          (+ (/ y 10) offset))))))))
      (fude-gl:in-shader instancing)
      (loop :for vec2 :in translations
            :for i :upfrom 0
            :do (gl:uniformfv
                  (gl:get-uniform-location (fude-gl::program-id instancing)
                                           (format nil "offsets[~A]" i))
                  vec2))
      (fude-gl::in-vertex-array vao))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t)
      (:idle ()
        (fude-gl:with-clear (win (:color-buffer-bit))
          (%gl:draw-arrays-instanced :triangles 0 6 (length translations)))))))

;;;; INSTANCED-ARRAYS

(fude-gl:defshader instanced-arrays-demo 330 (fude-gl:xy fude-gl:rgb
                                              fude-gl::offset)
  (:vertex ((|fColor| :vec3))
    "gl_Position = vec4(xy + offset, 0.0, 1.0);"
    "fColor = rgb;")
  (:fragment ((|fragColor| :vec4)) "fragColor = vec4(fColor, 1.0);"))

(defparameter *translations*
  (loop :with offset = 0.1
        :for y :upfrom -10 :below 10 :by 2
        :nconc (loop :for x :upfrom -10 :below 10 :by 2
                     :collect (list (+ (/ x 10) offset) (+ (/ y 10) offset)))
          :into result
        :finally (return
                  (make-array '(100 2)
                              :element-type 'single-float
                              :initial-contents result))))

;; Using low level abstructions.

#+|version0|
(defun instanced-arrays-demo ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl) :w 800 :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl::with-prog ((prog (fude-gl::vertex-shader
                                    'instanced-arrays-demo)
                               (fude-gl::fragment-shader
                                 'instanced-arrays-demo)))
          (fude-gl::in-shader prog)
          (fude-gl:with-gl-vector ((translations *translations*)
                                   (quad *instancing*))
            (fude-gl::with-buffer ((instance-buffer) (quad-buffer))
              (fude-gl::with-vertex-array ((vao
                                            ;; Initialize-buffer
                                            (fude-gl::in-buffer quad-buffer)
                                            (gl:buffer-data
                                              (fude-gl::buffer-target
                                                quad-buffer)
                                              (fude-gl::buffer-usage
                                                quad-buffer)
                                              quad)
                                            ;;; Initialize vertex array.
                                            ;; xy
                                            (fude-gl::in-buffer quad-buffer)
                                            (gl:enable-vertex-attrib-array 0)
                                            (gl:vertex-attrib-pointer 0 2
                                                                      :float
                                                                      nil
                                                                      (* 5
                                                                         (cffi:foreign-type-size
                                                                           :float))
                                                                      0)
                                            ;; rgb
                                            (fude-gl::in-buffer quad-buffer)
                                            (gl:enable-vertex-attrib-array 1)
                                            (gl:vertex-attrib-pointer 1 3
                                                                      :float
                                                                      nil
                                                                      (* 5
                                                                         (cffi:foreign-type-size
                                                                           :float))
                                                                      (* 2
                                                                         (cffi:foreign-type-size
                                                                           :float)))
                                            ;; offset
                                            (fude-gl::in-buffer
                                             instance-buffer)
                                            (gl:buffer-data
                                              (fude-gl::buffer-target
                                                instance-buffer)
                                              (fude-gl::buffer-usage
                                                instance-buffer)
                                              translations)
                                            (gl:enable-vertex-attrib-array 2)
                                            (gl:vertex-attrib-pointer 2 2
                                                                      :float
                                                                      nil
                                                                      (* 2
                                                                         (cffi:foreign-type-size
                                                                           :float))
                                                                      0)
                                            (%gl:vertex-attrib-divisor 2 1)))
                (sdl2:with-event-loop (:method :poll)
                  (:quit ()
                    t)
                  (:idle ()
                    (fude-gl:with-clear (win (:color-buffer-bit))
                      (%gl:draw-arrays-instanced :triangles 0 6 100))))))))))))

#+design
(fude-gl:defshader instanced-arrays-demo 330 (fude-gl:xy fude-gl:rgb &instance
                                              offset)
  (:vertex ((|fcolor| :vec3))
    "gl_position = vec4(xy + offset, 0.0, 1.0);"
    "fcolor = rgb;")
  (:fragment ((|fragcolor| :vec4)) "fragcolor = vec4(fcolor, 1.0);"))

;; Use helper functions.

#+|version1|
(defun instanced-arrays-demo ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl) :w 800 :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl::with-prog ((prog (fude-gl::vertex-shader
                                    'instanced-arrays-demo)
                               (fude-gl::fragment-shader
                                 'instanced-arrays-demo)))
          (fude-gl::in-shader prog)
          (fude-gl:with-gl-vector ((translations *translations*)
                                   (quad *instancing*))
            (fude-gl::with-buffer ((instance-buffer) (quad-buffer))
              (fude-gl::with-vertex-array ((vao
                                            ;; Initialize-buffer
                                            (fude-gl::send quad quad-buffer)
                                            (fude-gl::send translations
                                                           instance-buffer)
                                            ;;; Initialize vertex array.
                                            (fude-gl::in-buffer quad-buffer)
                                            ;; xy
                                            (fude-gl::link-attribute
                                              'fude-gl:xy
                                              'instanced-arrays-demo)
                                            ;; rgb
                                            (fude-gl::link-attribute
                                              'fude-gl:rgb
                                              'instanced-arrays-demo)
                                            ;; offset
                                            (fude-gl::in-buffer
                                             instance-buffer)
                                            (fude-gl::link-attribute
                                              'fude-gl::offset
                                              'instanced-arrays-demo)))
                (sdl2:with-event-loop (:method :poll)
                  (:quit ()
                    t)
                  (:idle ()
                    (fude-gl:with-clear (win (:color-buffer-bit))
                      (%gl:draw-arrays-instanced :triangles 0 6 100))))))))))))

;; Use higher level helper function.

#+|version2|
(defun instanced-arrays-demo ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl) :w 800 :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl::with-prog ((prog (fude-gl::vertex-shader
                                    'instanced-arrays-demo)
                               (fude-gl::fragment-shader
                                 'instanced-arrays-demo)))
          (fude-gl::in-shader prog)
          (fude-gl:with-gl-vector ((translations *translations*)
                                   (quad *instancing*))
            (fude-gl::with-buffer ((instance-buffer) (quad-buffer))
              (fude-gl::with-vertex-array ((vao
                                            (fude-gl::send quad quad-buffer)
                                            (fude-gl::send translations
                                                           instance-buffer)
                                            (fude-gl::in-buffer quad-buffer)
                                            (fude-gl::link-attributes
                                              'instanced-arrays-demo
                                              instance-buffer)))
                (sdl2:with-event-loop (:method :poll)
                  (:quit ()
                    t)
                  (:idle ()
                    (fude-gl:with-clear (win (:color-buffer-bit))
                      (%gl:draw-arrays-instanced :triangles 0 6 100))))))))))))

;; Use WITH-VAO.

#+|version3|
(defun instanced-arrays-demo ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl) :w 800 :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl::with-vao ((vao
                             (:shader instanced-arrays-demo
                              (fude-gl::vertex-shader 'instanced-arrays-demo)
                              (fude-gl::fragment-shader
                                'instanced-arrays-demo))
                             (:attributes 'instanced-arrays-demo)
                             (:vertices nil *instancing*)
                             (:instances (fude-gl::offset *translations*))))
          (sdl2:with-event-loop (:method :poll)
            (:quit ()
              t)
            (:idle ()
              (fude-gl:with-clear (win (:color-buffer-bit))
                (%gl:draw-arrays-instanced :triangles 0 6 100)))))))))

;; Use WITH-SHADER
;;#+|version4|

(defun instanced-arrays-demo ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl) :w 800 :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ((instanced-arrays-demo
                                (:vertices quad-buffer-var *instancing*)
                                (:instances (fude-gl::offset *translations*))))
          (sdl2:with-event-loop (:method :poll)
            (:quit ()
              t)
            (:idle ()
              (fude-gl:with-clear (win (:color-buffer-bit))
                (%gl:draw-arrays-instanced :triangles 0 6 100)))))))))

(fude-gl:defshader instance-id-demo 330 (fude-gl:xy fude-gl:rgb
                                         fude-gl::offset)
  (:vertex ((|fColor| :vec3))
    "gl_Position = vec4(xy * (gl_InstanceID / 100.0) + offset, 0.0, 1.0);"
    "fColor = rgb;")
  (:fragment ((|fragColor| :vec4)) "fragColor = vec4(fColor, 1.0);"))

(defun instance-id-demo ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl) :w 800 :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ((instance-id-demo
                                (:vertices quad-buffer-var *instancing*)
                                (:instances (fude-gl::offset *translations*))))
          (sdl2:with-event-loop (:method :poll)
            (:quit ()
              t)
            (:idle ()
              (fude-gl:with-clear (win (:color-buffer-bit))
                (%gl:draw-arrays-instanced :triangles 0 6 100)))))))))

(fude-gl:defshader some-instances-demo 330 (fude-gl:xy fude-gl:rgb
                                            fude-gl::offset fude-gl::a)
  (:vertex ((|fColor| :vec4))
    "gl_Position = vec4(xy + offset, 0.0, 1.0);"
    "fColor = vec4(rgb, a);")
  (:fragment ((|fragColor| :vec4)) "fragColor = fColor;"))

(defun make-random-alpha-array (size)
  (let ((result
         (make-array size :element-type 'single-float :initial-element 0.0))
        (unit (float (/ size)))
        (alpha 0.0))
    (dotimes (i size result) (setf (aref result i) alpha) (incf alpha unit))))

(defun some-instance-demo ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl) :w 800 :h 600)
      (sdl2:with-gl-context (context win)
        (gl:enable :blend)
        (gl:blend-func :src-alpha :one-minus-src-alpha)
        (fude-gl:with-shader ((some-instances-demo
                                (:vertices quad-buffer-var *instancing*)
                                (:instances (fude-gl::offset *translations*)
                                            (fude-gl::a
                                             (make-random-alpha-array
                                               (array-dimension *translations*
                                                                0))))))
          (sdl2:with-event-loop (:method :poll)
            (:quit ()
              t)
            (:idle ()
              (fude-gl:with-clear (win (:color-buffer-bit))
                (%gl:draw-arrays-instanced :triangles 0 6 100)))))))))