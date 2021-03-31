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
                                       depth-demo cubes cameras walk-around
                                       text instancing instanced-arrays-demo
                                       instance-id-demo some-instance-demo
                                       some-instance-dynamics))
    (quit ())))

;;;; HELLO-TRIANGLE

(fude-gl:defshader hello-triangle 330 (fude-gl:xy)
  (:vertex () "gl_Position = vec4(xy, 0.0, 1.0);")
  (:fragment ((|outColor| :vec4)) "outColor = vec4(1.0, 1.0, 1.0, 1.0);"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *triangle*
    (concatenate '(array single-float (*))
                 (make-instance 'fude-gl:xy :x 0.0 :y 0.5)
                 (make-instance 'fude-gl:xy :x 0.5 :y -0.5)
                 (make-instance 'fude-gl:xy :x -0.5 :y -0.5))))

(fude-gl:defvertices hello-triangle *triangle*)

(defun hello-triangle ()
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :w 800
                           :h 600
                           :title "Hello triangle"))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader () (fude-gl:in-vertices 'hello-triangle))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      (fude-gl:draw 'hello-triangle))))

;;;; UNIFORM-DEMO

(fude-gl:defshader uniform-demo 330 (fude-gl:xy)
  ;; Re-use vertex-shader of HELLO-TRIANGLE.
  (:vertex () 'hello-triangle)
  (:fragment ((|outColor| :vec4) &uniform (|triangleColor| :vec3))
    "outColor = vec4(triangleColor, 1.0);"))

(fude-gl:defvertices uniform-demo *triangle*)

(defun uniform-demo ()
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :title "Uniform demo"
                           :w 800
                           :h 600))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader ())
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t)
      (:idle ()
        (fude-gl:with-clear (win (:color-buffer-bit))
          (fude-gl:in-vertices 'uniform-demo)
          (gl:uniformf (fude-gl:uniform "triangleColor" 'uniform-demo)
                       (/ (+ 1.0 (sin (get-internal-real-time))) 2) 0.0 0.0)
          (fude-gl:draw 'uniform-demo))))))

;;;; COLORED-TRIANGLE

(fude-gl:defshader colored-triangle 330 (fude-gl:xy fude-gl:rgb)
  (:vertex ((color :vec3)) "color = rgb;" "gl_Position = vec4(xy, 0.0, 1.0);")
  (:fragment ((|outColor| :vec4)) "outColor = vec4(color, 1.0);"))

(fude-gl:defvertices colored-triangle
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
                           :title "Colored triangle"
                           :w 800
                           :h 600))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader ()
      (fude-gl:in-vertices 'colored-triangle)
      (sdl2:with-event-loop (:method :poll)
        (:quit ()
          t)
        (:idle ()
          (fude-gl:with-clear (win (:color-buffer-bit))
            (fude-gl:draw 'colored-triangle)))))))

;;;; ELEMENT-BUFFER

(fude-gl:defvertices element-buffer
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
                                :b 1.0))
  :shader 'colored-triangle
  :indices (list '(0 1 2 2 3 0)))

(defun element-buffer ()
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :title "Element buffer"
                           :w 800
                           :h 600))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader () (fude-gl:in-vertices 'element-buffer))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      (fude-gl:draw 'element-buffer))))

;;;; TEXTURE

(fude-gl:defshader texture-demo 330 (fude-gl:xy fude-gl:rgb fude-gl:st)
  (:vertex ((color :vec3) (texcoord :vec2))
    "texcoord = st;"
    "color = rgb;"
    "gl_Position = vec4(xy, 0.0, 1.0);")
  (:fragment ((|outColor| :vec4) &uniform (tex :|sampler2D|))
    "outColor = texture(tex, texcoord) * vec4(color, 1.0);"))

(defparameter *png*
  (opticl:read-png-file
    (probe-file
      (merge-pathnames "examples/lisplogo_alien_128.png"
                       (asdf:system-source-directory
                         (asdf:find-system :fude-gl-examples))))))

(fude-gl:deftexture lisp-alien :texture-2d (fude-gl:tex-image-2d *png*))

(fude-gl:defvertices texture-demo
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
                                :t 1.0))
  :indices (list '(0 1 2 2 3 0)))

(defun texture-demo ()
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :title "Texture demo"
                           :w 800
                           :h 600))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader ())
    (fude-gl:with-textures nil
      (fude-gl:in-vertices 'texture-demo)
      (fude-gl:in-texture 'lisp-alien))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      (fude-gl:draw 'texture-demo))))

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

(defparameter *logo*
  (opticl:read-png-file
    (probe-file
      (merge-pathnames "examples/lisplogo_warning2_128.png"
                       (asdf:system-source-directory
                         (asdf:find-system :fude-gl-examples))))))

(fude-gl:deftexture lisp-logo :texture-2d (fude-gl:tex-image-2d *logo*))

(fude-gl:defvertices mix-demo
    (concatenate '(array single-float (*))
                 (make-instance 'mix-demo :x -0.5 :y 0.5 :s 0.0 :t 0.0)
                 (make-instance 'mix-demo :x 0.5 :y 0.5 :s 1.0 :t 0.0)
                 (make-instance 'mix-demo :x 0.5 :y -0.5 :s 1.0 :t 1.0)
                 (make-instance 'mix-demo :x -0.5 :y -0.5 :s 0.0 :t 1.0))
  :indices (list '(0 1 2 2 3 0)))

(defun mix-demo ()
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :title "Mix demo"
                           :w 800
                           :h 600))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader ())
    (fude-gl:with-textures nil)
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      (fude-gl:connect 'mix-demo "tex1" 'lisp-alien "tex2" 'lisp-logo)
      (fude-gl:draw 'mix-demo))))

;;;; HELLO from glut-examples.

(fude-gl:defshader hello 330 (fude-gl:xy)
  (:vertex () "gl_Position = vec4(xy,0.0,1.0);")
  (:fragment ((color :vec4)) "color = vec4(1.0, 1.0, 1.0, 1.0);"))

(fude-gl:defvertices hello
    (concatenate '(array single-float (*))
                 (make-instance 'hello :x -0.5 :y 0.5) ; top-left
                 (make-instance 'hello :x 0.5 :y 0.5) ; top-right
                 (make-instance 'hello :x -0.5 :y -0.5) ; bottom-left
                 (make-instance 'hello :x 0.5 :y -0.5))
  :indices (list '(0 1 2 2 3 1)))

(defun hello ()
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :w 250
                           :h 250
                           :title "hello"))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader () (fude-gl:in-vertices 'hello))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit)) (fude-gl:draw 'hello))))

;;;; DOUBLE from glut-examples.

(fude-gl:defshader double 330 (fude-gl:xyz)
  (:vertex ((|texCoord| :vec2) &uniform (transform :mat4))
    "gl_Position = transform * vec4(xyz, 1.0);")
  (:fragment ((color :vec4)) "color = vec4(1.0, 1.0, 1.0, 1.0);"))

(fude-gl:defvertices double
    (concatenate '(array single-float (*))
                 (make-instance 'double :x -0.5 :y 0.5 :z 0.0) ; top-left
                 (make-instance 'double :x 0.5 :y 0.5 :z 0.0) ; top-right
                 (make-instance 'double :x -0.5 :y -0.5 :z 0.0) ; bottom-left
                 (make-instance 'double :x 0.5 :y -0.5 :z 0.0))
  :indices `((0 1 2 2 3 1)))

(defun double ()
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :w 250
                           :h 250
                           :title "double"))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader () (fude-gl:in-vertices 'double))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit) :color '(0.2 0.3 0.3 1.0))
      (gl:uniform-matrix (fude-gl:uniform "transform" 'double) 4
                         (vector
                           (3d-matrices:marr
                             (3d-matrices:nmrotate (3d-matrices:meye 4)
                                                   3d-vectors:+vz+
                                                   (fude-gl:radians
                                                     (get-internal-real-time))))))
      (fude-gl:draw 'double))))

;;;; MATRIX-OPERATIONS

(defparameter *image*
  (let ((pathname (merge-pathnames "container.jpg" (user-homedir-pathname))))
    (unless (probe-file pathname)
      (dex:fetch "https://learnopengl.com/img/textures/container.jpg"
                 pathname))
    (opticl:read-jpeg-file pathname)))

(fude-gl:deftexture container :texture-2d (fude-gl:tex-image-2d *image*))

(defparameter *face*
  (let ((pathname (merge-pathnames "awesomeface.png" (user-homedir-pathname))))
    (unless (probe-file pathname)
      (dex:fetch "https://learnopengl.com/img/textures/awesomeface.png"
                 pathname))
    (opticl:vertical-flip-image (opticl:read-png-file pathname))))

(fude-gl:deftexture face :texture-2d (fude-gl:tex-image-2d *face*))

(fude-gl:defshader transform-demo 330 (fude-gl:xy fude-gl:st)
  (:vertex ((coord :vec2) &uniform (transform :mat4))
    "gl_Position = transform * vec4(xy, 0.0, 1.0);"
    "coord = st;")
  (:fragment ((color :vec4) &uniform (tex1 :|sampler2D|) (tex2 :|sampler2D|))
    "color = mix(texture(tex1, coord), texture(tex2, coord), 0.2);"))

(fude-gl:defvertices transform-demo
    (concatenate '(array single-float (*))
                 (make-instance 'transform-demo :x -0.5 :y 0.5 :s 0.0 :t 1.0) ; top
                                                                              ; left
                 (make-instance 'transform-demo :x 0.5 :y 0.5 :s 1.0 :t 1.0) ; top
                                                                             ; right
                 (make-instance 'transform-demo :x -0.5 :y -0.5 :s 0.0 :t 0.0) ; bottom
                                                                               ; left
                 (make-instance 'transform-demo :x 0.5 :y -0.5 :s 1.0 :t 0.0))
  :indices `((0 1 2 2 3 1)))

(defun transform-demo ()
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :title "Transform demo"
                           :w 800
                           :h 600))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader ())
    (fude-gl:with-textures nil
      (fude-gl:in-vertices 'transform-demo))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      (fude-gl:connect 'transform-demo "tex1" 'container "tex2" 'face)
      (gl:uniform-matrix (fude-gl:uniform "transform" 'transform-demo) 4
                         (vector
                           (3d-matrices:marr
                             (3d-matrices:nmscale
                               (3d-matrices:nmrotate (3d-matrices:meye 4)
                                                     3d-vectors:+vz+
                                                     (fude-gl:radians 90))
                               (3d-vectors:vec 0.5 0.5 0.5)))))
      (fude-gl:draw 'transform-demo))))

(defun translate-x ()
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :title "Translate x"
                           :w 800
                           :h 600))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader ())
    (fude-gl:with-textures nil
      (fude-gl:in-vertices 'transform-demo))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      (fude-gl:connect 'transform-demo "tex1" 'container "tex2" 'face)
      (gl:uniform-matrix (fude-gl:uniform "transform" 'transform-demo) 4
                         (vector
                           (3d-matrices:marr
                             (3d-matrices:mtranslation
                               (3d-vectors:vec (sin (get-internal-real-time))
                                               0.0 0.0)))))
      (fude-gl:draw 'transform-demo))))

(defun translate-y ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :title "Translate y"
                           :w 800
                           :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ()
          (fude-gl:with-textures nil
            (fude-gl:in-vertices 'transform-demo)
            (sdl2:with-event-loop (:method :poll)
              (:quit ()
                t)
              (:idle ()
                (fude-gl:with-clear (win (:color-buffer-bit))
                  (fude-gl:connect 'transform-demo "tex1" 'container "tex2"
                                   'face)
                  (gl:uniform-matrix
                    (fude-gl:uniform "transform" 'transform-demo) 4
                    (vector
                      (3d-matrices:marr
                        (3d-matrices:mtranslation
                          (3d-vectors:vec 0.0 (sin (get-internal-real-time))
                                          0.0)))))
                  (fude-gl:draw 'transform-demo))))))))))

(defun scaling ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :title "Scaling"
                           :w 800
                           :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ()
          (fude-gl:with-textures nil
            (fude-gl:in-vertices 'transform-demo)
            (sdl2:with-event-loop (:method :poll)
              (:quit ()
                t)
              (:idle ()
                (fude-gl:with-clear (win (:color-buffer-bit))
                  (fude-gl:connect 'transform-demo "tex1" 'container "tex2"
                                   'face)
                  (gl:uniform-matrix
                    (fude-gl:uniform "transform" 'transform-demo) 4
                    (vector
                      (3d-matrices:marr
                        (3d-matrices:nmscale
                          (3d-matrices:mtranslation (3d-vectors:vec 0 0 0))
                          (let ((v (abs (sin (get-internal-real-time)))))
                            (3d-vectors:vec v v 0.0))))))
                  (fude-gl:draw 'transform-demo))))))))))

(defun rotating ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :title "Rotating"
                           :w 800
                           :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ()
          (fude-gl:with-textures nil
            (fude-gl:in-vertices 'transform-demo)
            (sdl2:with-event-loop (:method :poll)
              (:quit ()
                t)
              (:idle ()
                (fude-gl:with-clear (win (:color-buffer-bit))
                  (fude-gl:connect 'transform-demo "tex1" 'container "tex2"
                                   'face)
                  (gl:uniform-matrix
                    (fude-gl:uniform "transform" 'transform-demo) 4
                    (vector
                      (3d-matrices:marr
                        (3d-matrices:nmrotate
                          (3d-matrices:mtranslation (3d-vectors:vec 0 0 0))
                          3d-vectors:+vz+
                          (fude-gl:radians (get-internal-real-time))))))
                  (fude-gl:draw 'transform-demo))))))))))

;;;; COORD-DEMO

(fude-gl:defshader coord-demo 330 (fude-gl:xy fude-gl:st)
  (:vertex ((coord :vec2) &uniform (model :mat4) (view :mat4)
            (projection :mat4))
    "gl_Position = projection * view * model * vec4(xy, 0.0, 1.0);"
    "coord = st;")
  (:fragment ((color :vec4) &uniform (tex1 :|sampler2D|) (tex2 :|sampler2D|))
    "color = mix(texture(tex1, coord), texture(tex2, coord), 0.2);"))

(fude-gl:defvertices coord-demo
    (concatenate '(array single-float (*))
                 (make-instance 'coord-demo :x -0.5 :y 0.5 :s 0.0 :t 1.0) ; top
                                                                          ; left
                 (make-instance 'coord-demo :x 0.5 :y 0.5 :s 1.0 :t 1.0) ; top
                                                                         ; right
                 (make-instance 'coord-demo :x -0.5 :y -0.5 :s 0.0 :t 0.0) ; bottom
                                                                           ; left
                 (make-instance 'coord-demo :x 0.5 :y -0.5 :s 1.0 :t 0.0))
  :indices `((0 1 2 2 3 1)))

(defun coord-demo ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :title "Coord demo"
                           :w 800
                           :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ()
          (fude-gl:with-textures nil
            (fude-gl:in-vertices 'coord-demo)
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
                       (gl:uniform-matrix (fude-gl:uniform uniform 'coord-demo)
                                          4
                                          (vector (3d-matrices:marr matrix)))))
                (sdl2:with-event-loop (:method :poll)
                  (:quit ()
                    t)
                  (:idle ()
                    (fude-gl:with-clear (win (:color-buffer-bit))
                      (fude-gl:connect 'coord-demo "tex1" 'container "tex2"
                                       'face)
                      (send m "model")
                      (send v "view")
                      (send p "projection")
                      (fude-gl:draw 'coord-demo))))))))))))

;;;; DEPTH-DEMO

(fude-gl:defshader depth-demo 330 (fude-gl:xyz fude-gl:st)
  (:vertex ((coord :vec2) &uniform (model :mat4) (view :mat4)
            (projection :mat4))
    "gl_Position = projection * view * model * vec4(xyz, 1.0);"
    "coord = st;")
  (:fragment ((color :vec4) &uniform (tex1 :|sampler2D|) (tex2 :|sampler2D|))
    "color = mix(texture(tex1, coord), texture(tex2, coord), 0.2);"))

(eval-when (:compile-toplevel :load-toplevel :execute)
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
                   (make -0.5 0.5 0.5 0.0 0.0) (make -0.5 0.5 -0.5 0.0 1.0)))))

(fude-gl:defvertices depth-demo *depth-demo*)

(defun depth-demo ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :title "Depth demo"
                           :w 800
                           :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ()
          (fude-gl:with-textures nil
            (fude-gl:in-vertices 'depth-demo)
            (flet ((send (matrix uniform)
                     (gl:uniform-matrix (fude-gl:uniform uniform 'depth-demo) 4
                                        (vector (3d-matrices:marr matrix)))))
              (gl:enable :depth-test)
              (sdl2:with-event-loop (:method :poll)
                (:quit ()
                  t)
                (:idle ()
                  (fude-gl:with-clear (win (:color-buffer-bit :depth-buffer-bit))
                    (fude-gl:connect 'depth-demo "tex1" 'container "tex2"
                                     'face)
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
                      (send m "model")
                      (send v "view")
                      (send p "projection")
                      (fude-gl:draw 'depth-demo))))))))))))

;;;; CUBES

(fude-gl:defshader cubes 330 (fude-gl:xyz fude-gl:st)
  (:vertex ((coord :vec2) &uniform (model :mat4) (view :mat4)
            (projection :mat4))
    "gl_Position = projection * view * model * vec4(xyz, 1.0);"
    "coord = st;")
  (:fragment ((color :vec4) &uniform (tex1 :|sampler2D|) (tex2 :|sampler2D|))
    "color = mix(texture(tex1, coord), texture(tex2, coord), 0.2);"))

(fude-gl:defvertices cubes *depth-demo*)

(defun cubes ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :title "Cubes"
                           :w 800
                           :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ()
          (fude-gl:with-textures nil
            (fude-gl:in-vertices 'cubes)
            (flet ((send (matrix uniform)
                     (gl:uniform-matrix (fude-gl:uniform uniform 'cubes) 4
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
                      (fude-gl:connect 'cubes "tex1" 'container "tex2" 'face)
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
                                  (send m "model")
                                  (send v "view")
                                  (send p "projection")
                                  (fude-gl:draw 'cubes))))))))))))))

;;;; CAMERAS

(defun cameras ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :title "Cameras"
                           :w 800
                           :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ()
          (fude-gl:with-textures nil
            (fude-gl:in-vertices 'cubes)
            (flet ((send (matrix uniform)
                     (gl:uniform-matrix (fude-gl:uniform uniform 'cubes) 4
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
                      (fude-gl:connect 'cubes "tex1" 'container "tex2" 'face)
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
                                    (send m "model")
                                    (send v "view")
                                    (send p "projection")
                                    (fude-gl:draw 'cubes)))))))))))))))

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
                           :title "Walk around"
                           :w 800
                           :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ()
          (fude-gl:with-textures nil
            (fude-gl:in-vertices 'cubes)
            (flet ((send (matrix uniform)
                     (gl:uniform-matrix (fude-gl:uniform uniform 'cubes) 4
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
                      (fude-gl:connect 'cubes "tex1" 'container "tex2" 'face)
                      (loop :for pos :in *cube-positions*
                            :for i :upfrom 0
                            :for m
                                 = (3d-matrices:nmrotate
                                     (3d-matrices:mtranslation pos)
                                     (3d-vectors:vec 1 0.3 0.5)
                                     (fude-gl:radians (* 20 i)))
                            :do (send m "model")
                                (send
                                  (3d-matrices:mlookat camera-pos
                                                       (3d-vectors:v+
                                                         camera-pos
                                                         camera-front)
                                                       camera-up)
                                  "view")
                                (send p "projection")
                                (fude-gl:draw 'cubes)))))))))))))

;;;; FONT

(defun text ()
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :w 800
                           :h 600
                           :title "Text"))
    (sdl2:with-gl-context (context win)
      (gl:enable :blend)
      (gl:blend-func :src-alpha :one-minus-src-alpha))
    (fude-gl:with-text-renderer (renderer :size 32 :win win))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t)
      (:idle ()
        (fude-gl:with-clear (win (:color-buffer-bit))
          (renderer "Hello world! g" :x 0 :y :center))))))

;;;; INSTANCING

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *instancing*
    (coerce
      #(-0.05 0.05 1.0 0.0 0.0 ; first
        0.05 -0.05 0.0 1.0 0.0 ; second
        -0.05 -0.05 0.0 0.0 1.0 ; third
        -0.05 0.05 1.0 0.0 0.0 ; fourth
        0.05 -0.05 0.0 1.0 0.0 ; fifth
        0.05 0.05 0.0 1.0 1.0 ; sixth
        )
      '(array single-float (*)))))

(fude-gl:defshader instancing 330 (fude-gl:xy fude-gl:rgb)
  (:vertex ((|fColor| :vec3) &uniform (offsets :vec2 100))
    "vec2 offset = offsets[gl_InstanceID];"
    "gl_Position = vec4(xy + offset, 0.0, 1.0);"
    "fColor = rgb;")
  (:fragment ((|fragColor| :vec4)) "fragColor = vec4(fColor, 1.0);"))

(fude-gl:defvertices instancing *instancing*)

(defun instancing ()
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :w 800
                           :h 600
                           :title "Instancing"))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader ())
    (let ((translations
           (uiop:while-collecting (acc)
             (loop :with offset = 0.1
                   :for y :upfrom -10 :below 10 :by 2
                   :do (loop :for x :upfrom -10 :below 10 :by 2
                             :do (acc
                                  (vector (+ (/ x 10) offset)
                                          (+ (/ y 10) offset))))))))
      (fude-gl:in-vertices 'instancing)
      (loop :for vec2 :in translations
            :for i :upfrom 0
            :do (gl:uniformfv
                  (fude-gl:uniform (format nil "offsets[~A]" i) 'instancing)
                  vec2))
      (gl:bind-vertex-array (fude-gl:vertex-array 'instancing)))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t)
      (:idle ()
        (fude-gl:with-clear (win (:color-buffer-bit))
          (%gl:draw-arrays-instanced :triangles 0 6 (length translations)))))))

;;;; INSTANCED-ARRAYS

(fude-gl:defshader instanced-arrays-demo 330 (fude-gl:xy fude-gl:rgb
                                              fude-gl:offset)
  (:vertex ((|fColor| :vec3))
    "gl_Position = vec4(xy + offset, 0.0, 1.0);"
    "fColor = rgb;")
  (:fragment ((|fragColor| :vec4)) "fragColor = vec4(fColor, 1.0);"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *translations*
    (loop :with offset = 0.1
          :for y :upfrom -10 :below 10 :by 2
          :nconc (loop :for x :upfrom -10 :below 10 :by 2
                       :collect (list (+ (/ x 10) offset) (+ (/ y 10) offset)))
            :into result
          :finally (return
                    (make-array '(100 2)
                                :element-type 'single-float
                                :initial-contents result)))))

(fude-gl:defvertices instanced-arrays-demo *instancing*
  :instances `((fude-gl:offset ,*translations*)))

(defun instanced-arrays-demo ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :w 800
                           :h 600
                           :title "Instanced arrays demo")
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ()
          (fude-gl:in-vertices 'instanced-arrays-demo)
          (sdl2:with-event-loop (:method :poll)
            (:quit ()
              t)
            (:idle ()
              (fude-gl:with-clear (win (:color-buffer-bit))
                (fude-gl:draw 'instanced-arrays-demo)))))))))

;;;; INSTANCE-ID-DEMO

(fude-gl:defshader instance-id-demo 330 (fude-gl:xy fude-gl:rgb fude-gl:offset)
  (:vertex ((|fColor| :vec3))
    "gl_Position = vec4(xy * (gl_InstanceID / 100.0) + offset, 0.0, 1.0);"
    "fColor = rgb;")
  (:fragment ((|fragColor| :vec4)) "fragColor = vec4(fColor, 1.0);"))

(fude-gl:defvertices instance-id-demo *instancing*
  :instances `((fude-gl:offset ,*translations*)))

(defun instance-id-demo ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :w 800
                           :h 600
                           :title "InstanceID demo")
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ()
          (fude-gl:in-vertices 'instance-id-demo)
          (sdl2:with-event-loop (:method :poll)
            (:quit ()
              t)
            (:idle ()
              (fude-gl:with-clear (win (:color-buffer-bit))
                (fude-gl:draw 'instance-id-demo)))))))))

;;;; SOME-INSTANCES-DEMO

(fude-gl:defshader some-instances-demo 330 (fude-gl:xy fude-gl:rgb
                                            fude-gl:offset fude-gl:a)
  (:vertex ((|fColor| :vec4))
    "gl_Position = vec4(xy + offset, 0.0, 1.0);"
    "fColor = vec4(rgb, a);")
  (:fragment ((|fragColor| :vec4)) "fragColor = fColor;"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-random-alpha-array (size)
    (let ((result
           (make-array size :element-type 'single-float :initial-element 0.0))
          (unit (float (/ size)))
          (alpha 0.0))
      (dotimes (i size result)
        (setf (aref result i) alpha)
        (incf alpha unit)))))

(fude-gl:defvertices some-instances-demo *instancing*
  :instances `((fude-gl:offset ,*translations*)
               (fude-gl:a
                ,(make-random-alpha-array (array-dimension *translations* 0)))))

(defun some-instance-demo ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :w 800
                           :h 600
                           :title "Some instance demo")
      (sdl2:with-gl-context (context win)
        (gl:enable :blend)
        (gl:blend-func :src-alpha :one-minus-src-alpha)
        (fude-gl:with-shader ()
          (fude-gl:in-vertices 'some-instances-demo)
          (sdl2:with-event-loop (:method :poll)
            (:quit ()
              t)
            (:idle ()
              (fude-gl:with-clear (win (:color-buffer-bit))
                (fude-gl:draw 'some-instances-demo)))))))))

;;;; SOME-INSTANCE-DYNAMICS

(fude-gl:defvertices some-instance-dynamics *instancing*
  :shader 'some-instances-demo
  :instances `((fude-gl:offset ,*translations*)
               (fude-gl:a
                ,(make-array (array-dimension *translations* 0)
                             :element-type 'single-float
                             :initial-element 0.0)
                :usage :dynamic-draw)))

(defun some-instance-dynamics ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :w 800
                           :h 600
                           :title "Some instance dynamic")
      (sdl2:with-gl-context (context win)
        (gl:enable :blend)
        (gl:blend-func :src-alpha :one-minus-src-alpha)
        (fude-gl:with-shader ()
          (fude-gl:in-vertices 'some-instance-dynamics)
          (let* ((buffer
                  (fude-gl:instances-buffer 'some-instance-dynamics
                                            'fude-gl:a))
                 (vec (fude-gl:buffer-source buffer)))
            (sdl2:with-event-loop (:method :poll)
              (:quit ()
                t)
              (:idle ()
                (fude-gl:with-clear (win (:color-buffer-bit))
                  (setf (gl:glaref vec (random (gl::gl-array-size vec)))
                          (sin (get-internal-real-time)))
                  (fude-gl:in-buffer buffer)
                  (gl:buffer-sub-data (fude-gl:buffer-target buffer) vec)
                  (fude-gl:draw 'some-instance-dynamics))))))))))