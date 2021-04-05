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
;; Macro DEFSHADER defines shader.
;;
;; The first argument (HELLO-TRIANGLE in the example below) must a symbol
;; which acceptable for CL:DEFCLASS first argument.
;;
;; The second argument (330 in the example below) is GLSL version.
;;
;; The third argument ((xy) in the example below) is a list
;; which elements must be attributes.
;; You can see current defined attributes by evaluating (list-all-attributes).
;; Any attributes must defined by DEFINE-VERTEX-ATTRIBUTE.
;;
;; The rest arguments are shader clauses.
;;
;; The first element of the clause must one of :vertex or :fragment.
;;
;; The second element of the clause is called shader-lambda-list.
;; In the shader-lambda-list you can specify output spec.
;; In the example below, (|outColor| :vec4) is an output spec.
;;
;; The first element of the output spec must a symbol.
;; The second element of the output spec is a keyword symbol that names a GLSL type.
;;
;; The rest elements of the clause is GLSL source strings.

(fude-gl:defshader hello-triangle 330 (fude-gl:xy)
  (:vertex () "gl_Position = vec4(xy, 0.0, 1.0);")
  (:fragment ((|outColor| :vec4)) "outColor = vec4(1.0, 1.0, 1.0, 1.0);"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *triangle*
    (concatenate '(array single-float (*))
                 ;; The constructors of the ATTRIBUTE class makes vector.
                 (make-instance 'fude-gl:xy :x 0.0 :y 0.5)
                 (make-instance 'fude-gl:xy :x 0.5 :y -0.5)
                 (make-instance 'fude-gl:xy :x -0.5 :y -0.5))))

;; Macro DEFVERTICES defines vertices spec.
;;
;; The first argument (HELLO-TRIANGLE in the example below) must be a symbol.
;;
;; The second argument (*TRIANGLE* in the example below) must be a (array single-float (*)).
;;
;; Macro DEFVERTICES is evaluated in compile time,
;; so variable *TRIANGLE* needs to be wrapped by EVAL-WHEN.
;; (you can see above.)

(fude-gl:defvertices hello-triangle *triangle*)

(defun hello-triangle ()
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :w 800
                           :h 600
                           :title "Hello triangle"))
    (sdl2:with-gl-context (context win))
    ;; For cleanup, macro WITH-SHADER is recommended.
    (fude-gl:with-shader ()
      ;; To construct vertices.
      (fude-gl:in-vertices 'hello-triangle))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    ;; To clear window, WITH-CLEAR is recomended.
    (fude-gl:with-clear (win (:color-buffer-bit))
      ;; To draw vertices, you can use DRAW with passing vertices name.
      (fude-gl:draw 'hello-triangle))))

;;;; UNIFORM-DEMO

(fude-gl:defshader uniform-demo 330 (fude-gl:xy)
  ;; Re-use vertex-shader of HELLO-TRIANGLE.
  (:vertex () 'hello-triangle)
  ;; To use uniform, you must specify uniform-spec.
  ;; Lambda-list-keyword &UNIFORM is used to specify uniform-spec in the shader-lambda-list.
  ;; The first element of the uniform-spec must be a symbol.
  ;; The second element of the uniform-spec is a keyword symbol that names GLSL type.
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
          ;; To get an uniform locatioin, you can use a function UNIFORM.
          ;; The first argument must be a string that names uniform.
          ;; The second argument must be a symbol that names shader.
          (gl:uniformf (fude-gl:uniform "triangleColor" 'uniform-demo)
                       (/ (+ 1.0 (sin (get-internal-real-time))) 2) 0.0 0.0)
          (fude-gl:draw 'uniform-demo))))))

;;;; COLORED-TRIANGLE
;; You can specify some attributes in the attributes list.

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
  ;; When vertices name is not same with a shader name,
  ;; you must specify a shader to use.
  :shader 'colored-triangle
  ;; To enable indices.
  :indices
  (list '(0 1 2 2 3 0)))

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

;; Macto DEFTEXTURE defines a texture.
;;
;; The first argument (LISP-ALIEN in the example below) must be a symbol.
;;
;; The second argument (:TEXTURE-2D in the example below) must be a texture-target.
;;
;; The third argument is a form to initialize texture.

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
    ;; For cleanup textures, macro WITH-TEXTURES is recommended.
    (fude-gl:with-textures ()
      (fude-gl:in-vertices 'texture-demo)
      ;; To initialize texture.
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
    (fude-gl:with-textures ())
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      ;; When a shader needs some textures,
      ;; you can use a function CONNECT.
      ;; The first argument is a shader name.
      ;; The rest arguments are uniform name and texture name pairs.
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
      ;; To send matrix to the uniform variable,
      ;; you can use a generic function SEND.
      ;; In this case, the first argument is 3D-MATRICES:MAT4
      ;; The second argument is a shader name.
      ;; Aditinally keyword parameter :UNIFORM with specify an uniform name.
      (fude-gl:send
        (3d-matrices:nmrotate (3d-matrices:meye 4) 3d-vectors:+vz+
                              (fude-gl:radians (get-internal-real-time)))
        'double
        :uniform "transform")
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
    (fude-gl:with-textures () (fude-gl:in-vertices 'transform-demo))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      (fude-gl:connect 'transform-demo "tex1" 'container "tex2" 'face)
      (fude-gl:send
        (3d-matrices:nmscale
          (3d-matrices:nmrotate (3d-matrices:meye 4) 3d-vectors:+vz+
                                (fude-gl:radians 90))
          (3d-vectors:vec 0.5 0.5 0.5))
        'transform-demo
        :uniform "transform")
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
    (fude-gl:with-textures () (fude-gl:in-vertices 'transform-demo))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      (fude-gl:connect 'transform-demo "tex1" 'container "tex2" 'face)
      (fude-gl:send
        (3d-matrices:mtranslation
          (3d-vectors:vec (sin (get-internal-real-time)) 0.0 0.0))
        'transform-demo
        :uniform "transform")
      (fude-gl:draw 'transform-demo))))

(defun translate-y ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :title "Translate y"
                           :w 800
                           :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ()
          (fude-gl:with-textures ()
            (fude-gl:in-vertices 'transform-demo)
            (sdl2:with-event-loop (:method :poll)
              (:quit ()
                t)
              (:idle ()
                (fude-gl:with-clear (win (:color-buffer-bit))
                  (fude-gl:connect 'transform-demo "tex1" 'container "tex2"
                                   'face)
                  (fude-gl:send
                    (3d-matrices:mtranslation
                      (3d-vectors:vec 0.0 (sin (get-internal-real-time)) 0.0))
                    'transform-demo
                    :uniform "transform")
                  (fude-gl:draw 'transform-demo))))))))))

(defun scaling ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :title "Scaling"
                           :w 800
                           :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ()
          (fude-gl:with-textures ()
            (fude-gl:in-vertices 'transform-demo)
            (sdl2:with-event-loop (:method :poll)
              (:quit ()
                t)
              (:idle ()
                (fude-gl:with-clear (win (:color-buffer-bit))
                  (fude-gl:connect 'transform-demo "tex1" 'container "tex2"
                                   'face)
                  (fude-gl:send
                    (3d-matrices:nmscale
                      (3d-matrices:mtranslation (3d-vectors:vec 0 0 0))
                      (let ((v (abs (sin (get-internal-real-time)))))
                        (3d-vectors:vec v v 0.0)))
                    'transform-demo
                    :uniform "transform")
                  (fude-gl:draw 'transform-demo))))))))))

(defun rotating ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :title "Rotating"
                           :w 800
                           :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ()
          (fude-gl:with-textures ()
            (fude-gl:in-vertices 'transform-demo)
            (sdl2:with-event-loop (:method :poll)
              (:quit ()
                t)
              (:idle ()
                (fude-gl:with-clear (win (:color-buffer-bit))
                  (fude-gl:connect 'transform-demo "tex1" 'container "tex2"
                                   'face)
                  (fude-gl:send
                    (3d-matrices:nmrotate
                      (3d-matrices:mtranslation (3d-vectors:vec 0 0 0))
                      3d-vectors:+vz+
                      (fude-gl:radians (get-internal-real-time)))
                    'transform-demo
                    :uniform "transform")
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
          (fude-gl:with-textures ()
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
              (sdl2:with-event-loop (:method :poll)
                (:quit ()
                  t)
                (:idle ()
                  (fude-gl:with-clear (win (:color-buffer-bit))
                    (fude-gl:connect 'coord-demo "tex1" 'container "tex2"
                                     'face)
                    (fude-gl:send m 'coord-demo :uniform "model")
                    (fude-gl:send v 'coord-demo :uniform "view")
                    (fude-gl:send p 'coord-demo :uniform "projection")
                    (fude-gl:draw 'coord-demo)))))))))))

;;;; ORTHO-DEMO

(fude-gl:defshader ortho-demo 330 (fude-gl:xy fude-gl:st)
  (:vertex ((coord :vec2) &uniform (projection :mat4) (model :mat4))
    "gl_Position = projection * model * vec4(xy, 0.0, 1.0);"
    "coord = st;")
  (:fragment ((color :vec4) &uniform (tex :|sampler2D|))
    "color = texture(tex, coord);"))

(fude-gl:defvertices ortho-demo
    (concatenate '(array single-float (*))
                 (make-instance 'coord-demo :x -0.5 :y 0.5 :s 0.0 :t 1.0) ; top
                                                                          ; left
                 (make-instance 'coord-demo :x 0.5 :y 0.5 :s 1.0 :t 1.0) ; top
                                                                         ; right
                 (make-instance 'coord-demo :x -0.5 :y -0.5 :s 0.0 :t 0.0) ; bottom
                                                                           ; left
                 (make-instance 'coord-demo :x 0.5 :y -0.5 :s 1.0 :t 0.0))
  :indices `((0 1 2 2 3 1)))

(defun ortho-demo ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :title "Ortho demo"
                           :w 800
                           :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ()
          (fude-gl:with-textures ()
            (fude-gl:in-vertices 'ortho-demo)
            (let ((p (3d-matrices:mortho 0 800 0 600 -1 1))
                  (m (fude-gl::model-matrix 0 0 100 100)))
              (sdl2:with-event-loop (:method :poll)
                (:quit ()
                  t)
                (:idle ()
                  (fude-gl:with-clear (win (:color-buffer-bit))
                    (fude-gl::in-texture 'face)
                    (fude-gl:send m 'ortho-demo :uniform "model")
                    (fude-gl:send p 'ortho-demo :uniform "projection")
                    (fude-gl:draw 'ortho-demo)))))))))))

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
          (fude-gl:with-textures ()
            (fude-gl:in-vertices 'depth-demo)
            (gl:enable :depth-test)
            (sdl2:with-event-loop (:method :poll)
              (:quit ()
                t)
              (:idle ()
                (fude-gl:with-clear (win (:color-buffer-bit :depth-buffer-bit))
                  (fude-gl:connect 'depth-demo "tex1" 'container "tex2" 'face)
                  (let ((m
                         (3d-matrices:nmrotate (3d-matrices:meye 4)
                                               (3d-vectors:vec 0.5 1 0)
                                               (fude-gl:radians
                                                 (get-internal-real-time))))
                        (v (3d-matrices:mtranslation (3d-vectors:vec 0 0 -3)))
                        (p
                         (3d-matrices:mperspective 45
                                                   (multiple-value-call #'/
                                                     (sdl2:get-window-size
                                                       win))
                                                   0.1 100)))
                    (fude-gl:send m 'depth-demo :uniform "model")
                    (fude-gl:send v 'depth-demo :uniform "view")
                    (fude-gl:send p 'depth-demo :uniform "projection")
                    (fude-gl:draw 'depth-demo)))))))))))

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
          (fude-gl:with-textures ()
            (fude-gl:in-vertices 'cubes)
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
                                (fude-gl:send m 'cubes :uniform "model")
                                (fude-gl:send v 'cubes :uniform "view")
                                (fude-gl:send p 'cubes :uniform "projection")
                                (fude-gl:draw 'cubes)))))))))))))

;;;; CAMERAS

(defun cameras ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :title "Cameras"
                           :w 800
                           :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ()
          (fude-gl:with-textures ()
            (fude-gl:in-vertices 'cubes)
            (let ((cube-positions
                   (list (3d-vectors:vec 0 0 0) (3d-vectors:vec 2 5 -15)
                         (3d-vectors:vec -1.5 -2.2 -2.5)
                         (3d-vectors:vec -3.8 -2.0 -12.3)
                         (3d-vectors:vec 2.4 -0.4 -3.5)
                         (3d-vectors:vec -1.7 3 -7.5)
                         (3d-vectors:vec 1.3 -2 -2.5)
                         (3d-vectors:vec 1.5 2 -2.5)
                         (3d-vectors:vec 1.5 0.2 -1.5)
                         (3d-vectors:vec -1.3 1 -1.5)))
                  ;; To make camera, you can use a function MAKE-CAMERA.
                  (camera (fude-gl:make-camera)))
              (gl:enable :depth-test)
              (sdl2:with-event-loop (:method :poll)
                (:quit ()
                  t)
                (:idle ()
                  (fude-gl:with-clear (win (:color-buffer-bit :depth-buffer-bit))
                    (fude-gl:connect 'cubes "tex1" 'container "tex2" 'face)
                    (let* ((radius 10)
                           ;; To move camera you can use a function MOVE.
                           ;; The first argument is a camera object.
                           ;; The rest arguments are new X, Y and Z.
                           (moved
                            (fude-gl:move camera
                                          (* (sin (get-internal-real-time))
                                             radius)
                                          0
                                          (* (cos (get-internal-real-time))
                                             radius)))
                           ;; To get a view matrix, you can use a function VIEW.
                           ;; The first argument is a camera object.
                           ;; The keyword parameter :TARGET specifies to look at CAMERA-TARGET.
                           (v (fude-gl:view moved :target t)))
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
                                  (fude-gl:send m 'cubes :uniform "model")
                                  (fude-gl:send v 'cubes :uniform "view")
                                  (fude-gl:send p 'cubes :uniform "projection")
                                  (fude-gl:draw 'cubes))))))))))))))

;;;; WALK-AROUND

(defparameter *cube-positions*
  (list (3d-vectors:vec 0 0 0) (3d-vectors:vec 2 5 -15)
        (3d-vectors:vec -1.5 -2.2 -2.5) (3d-vectors:vec -3.8 -2.0 -12.3)
        (3d-vectors:vec 2.4 -0.4 -3.5) (3d-vectors:vec -1.7 3 -7.5)
        (3d-vectors:vec 1.3 -2 -2.5) (3d-vectors:vec 1.5 2 -2.5)
        (3d-vectors:vec 1.5 0.2 -1.5) (3d-vectors:vec -1.3 1 -1.5)))

(defun move-camera (keysym camera)
  (let ((camera-speed 0.05))
    (case (sdl2:scancode keysym)
      ;; To modify CAMERA-POSITION,
      ;; you should use 3D-VECTORS's N prefixed functions.
      (:scancode-up
       (3d-vectors:nv+ (fude-gl:camera-position camera)
                       (3d-vectors:v* camera-speed
                                      (fude-gl:camera-front camera))))
      (:scancode-down
       (3d-vectors:nv- (fude-gl:camera-position camera)
                       (3d-vectors:v* camera-speed
                                      (fude-gl:camera-front camera))))
      (:scancode-left
       (3d-vectors:nv- (fude-gl:camera-position camera)
                       (3d-vectors:v*
                         (3d-vectors:vunit
                           (3d-vectors:vc (fude-gl:camera-front camera)
                                          (fude-gl:camera-up camera)))
                         camera-speed)))
      (:scancode-right
       (3d-vectors:nv+ (fude-gl:camera-position camera)
                       (3d-vectors:v*
                         (3d-vectors:vunit
                           (3d-vectors:vc (fude-gl:camera-front camera)
                                          (fude-gl:camera-up camera)))
                         camera-speed))))))

(defun walk-around ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :title "Walk around"
                           :w 800
                           :h 600)
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ()
          (fude-gl:with-textures ()
            (fude-gl:in-vertices 'cubes)
            (let ((camera (fude-gl:make-camera))
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
                  (move-camera keysym camera))
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
                          :do (fude-gl:send m 'cubes :uniform "model")
                              (fude-gl:send (fude-gl:view camera) 'cubes
                                            :uniform "view")
                              (fude-gl:send p 'cubes :uniform "projection")
                              (fude-gl:draw 'cubes))))))))))))

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
    ;; To render text, you can use a macro WITH-TEXT-RENDERER.
    ;; The first argument is spec.
    ;; The first element of the spec must be a symbol that will be a function.
    ;; The keyword parameter :SIZE specify font size.
    ;; The required keyword parameter :WIN specify sdl2 window.
    (fude-gl:with-text-renderer (renderer :size 32 :win win))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t)
      (:idle ()
        (fude-gl:with-clear (win (:color-buffer-bit))
          ;; The first arument of the text renderer is a string to render.
          ;; The keyword parameter :X and :Y specifies position.
          ;; You can use :CENTER to specify centering the text.
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
  ;; When you want to use array for uniform variable,
  ;; you can specify its size in the third element of the uniform spec.
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
            :do (fude-gl:send vec2 'instancing
                              :uniform (format nil "offsets[~A]" i)))
      (gl:bind-vertex-array (fude-gl:vertex-array 'instancing)))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t)
      (:idle ()
        (fude-gl:with-clear (win (:color-buffer-bit))
          ;; NOTE: Could not use fude-gl:draw
          ;; due to instancing is not INSTANCED-VERTEX class.
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
  ;; To specify GPU instancing,
  ;; The value is attribute and array pair alist.
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
                ;; To specify buffer usage.
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
          (let ((vec
                  ;; To get gl-array object.
                 (fude-gl:buffer-source
                   ;; To get buffer object.
                   ;; The first argument is a vertices name.
                   ;; The second argument is an attribute name.
                   (fude-gl:instances-buffer 'some-instance-dynamics
                                             'fude-gl:a))))
            (sdl2:with-event-loop (:method :poll)
              (:quit ()
                t)
              (:idle ()
                (fude-gl:with-clear (win (:color-buffer-bit))
                  (setf (gl:glaref vec (random (gl::gl-array-size vec)))
                          (sin (get-internal-real-time)))
                  ;; To update.
                  (fude-gl:send 'fude-gl:a 'some-instance-dynamics)
                  (fude-gl:draw 'some-instance-dynamics))))))))))

;;;; FRAMEBUFFER

(fude-gl:defshader framebuffer-vertices 330 (fude-gl:xyz fude-gl:st)
  (:vertex ((coord :vec2) &uniform (model :mat4) (view :mat4)
            (projection :mat4))
    "gl_Position = projection * view * model * vec4(xyz, 1.0);"
    "coord = st;")
  (:fragment ((color :vec4) &uniform (tex :|sampler2D|))
    "color = texture(tex, coord);"))

(fude-gl:defvertices framebuffer-quad
    (concatenate '(array single-float (*)) #(-1.0 1.0 0.0 1.0)
                 #(-1.0 -1.0 0.0 0.0) #(1.0 -1.0 1.0 0.0) #(-1.0 1.0 0.0 1.0)
                 #(1.0 -1.0 1.0 0.0) #(1.0 1.0 1.0 1.0))
  :shader 'framebuffer-screen)

(fude-gl:defshader framebuffer-screen 330 (fude-gl:xy fude-gl:st)
  (:vertex ((coord :vec2)) "gl_Position = vec4(xy, 0.0, 1.0);" "coord = st;")
  (:fragment ((color :vec4) &uniform (screen :|sampler2D|))
    "color = vec4(texture(screen, coord).rgb, 1.0);"))

(fude-gl:defvertices fb-cube *depth-demo* :shader 'framebuffer-vertices)

(fude-gl:defvertices plane-vertices
    (concatenate '(array single-float (*)) #(5.0 -0.5 5.0 2.0 0.0)
                 #(-5.0 -0.5 5.0 0.0 0.0) #(-5.0 -0.5 -5.0 0.0 2.0) #()
                 #(5.0 -0.5 5.0 2.0 0.0) #(-5.0 -0.5 -5.0 0.0 2.0)
                 #(5.0 -0.5 -5.0 2.0 2.0))
  :shader 'framebuffer-vertices)

;; Macro DEFRAMEBUF defines framebuffer.
;;
;; The first argument (STEP1 in the example below) must be a symbol names framebuffer.

(fude-gl::deframebuf step1 :width 800 :height 600)

(defparameter *metal*
  (let ((pathname (merge-pathnames "metal.png" (user-homedir-pathname))))
    (unless (probe-file pathname)
      (dex:fetch
        "https://raw.githubusercontent.com/JoeyDeVries/LearnOpenGL/master/resources/textures/metal.png"
        pathname))
    (opticl:read-png-file pathname)))

(fude-gl:deftexture metal :texture-2d (fude-gl:tex-image-2d *metal*))

(defun framebuffer-step1 ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :w 800
                           :h 600
                           :title "Frame buffer Step1")
      (sdl2:with-gl-context (context win)
        (fude-gl:with-shader ()
          (gl:enable :depth-test)
          (let* ((camera (fude-gl:make-camera))
                 (view (fude-gl:view camera))
                 (projection (3d-matrices:mperspective 45 (/ 800 600) 0.1 100)))
            (fude-gl:send 0 'framebuffer-screen :uniform "screen")
            (fude-gl:send 0 'framebuffer-vertices :uniform "tex")
            (fude-gl:send view 'framebuffer-vertices :uniform "view")
            (fude-gl:send projection 'framebuffer-vertices
                          :uniform "projection")
            (sdl2:with-event-loop (:method :poll)
              (:quit ()
                t)
              (:idle ()
                (sleep (/ 1 30))
                ;; bind to framebuffer and draw scene as we normally would to color texture
                ;; For cleanup, macro WITH-FRAMEBUFFER is recommended.
                (fude-gl::with-framebuffer (step1 (:color-buffer-bit :depth-buffer-bit)
                                                  :color '(0.1 0.1 0.1 1))
                  (gl:enable :depth-test)
                  ;;; cubes
                  (fude-gl:in-vertices 'fb-cube)
                  (fude-gl:in-vertex-array (fude-gl:vertex-array 'fb-cube))
                  (gl:active-texture 0)
                  (fude-gl:in-texture 'container)
                  (fude-gl:send
                    (3d-matrices:mtranslation (3d-vectors:vec3 -1 0 -1))
                    'framebuffer-vertices
                    :uniform "model")
                  (fude-gl:draw 'fb-cube)
                  (fude-gl:send
                    (3d-matrices:mtranslation (3d-vectors:vec3 2 0 0))
                    'framebuffer-vertices
                    :uniform "model")
                  (fude-gl:draw 'fb-cube)
                  ;;; floor
                  (fude-gl:in-vertices 'plane-vertices)
                  (fude-gl:in-vertex-array
                    (fude-gl:vertex-array 'plane-vertices))
                  (fude-gl:in-texture 'metal)
                  (fude-gl:send (3d-matrices:meye 4) 'framebuffer-vertices
                                :uniform "model")
                  (fude-gl:draw 'plane-vertices))
                ;; draw a quad plane with the attached framebuffer color texture
                ;; disable depth test so screen-space quad isn't discarded due to depth test.
                (gl:disable :depth-test)
                ;; clear all relevant buffers
                ;; set clear color to white (not really necessary actually, since
                ;; we won't be able to see behind the quad anyways)
                (gl:clear-color 1 1 1 1)
                (gl:clear :color-buffer-bit)
                (fude-gl:in-vertices 'framebuffer-quad)
                (fude-gl:in-vertex-array
                  (fude-gl:vertex-array 'framebuffer-quad))
                (gl:bind-texture :texture-2d (fude-gl::framebuffer-texture
                                               (fude-gl::find-framebuffer
                                                 'step1)))
                (fude-gl:draw 'framebuffer-quad)
                (sdl2:gl-swap-window win)))))))))
