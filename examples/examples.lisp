(defpackage :fude-gl-examples
  (:use :cl)
  (:export #:demos))

(in-package :fude-gl-examples)

;;;; INTERFACE

(defun demos ()
  (restart-case (mapc #'funcall
                      '(hello-triangle uniform-demo colored-triangle
                                       element-buffer texture-demo mix-demo
                                       transform-demo translate-x translate-y
                                       scaling rotating coord-demo depth-demo
                                       cubes cameras walk-around look-around
                                       text instancing instanced-arrays-demo
                                       instance-id-demo some-instance-demo
                                       some-instance-dynamics depth-testing
                                       framebuffer-step1 framebuffer-shadow
                                       framebuffer-shadow-base))
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
;; In the shader-lambda-list you can specify io spec.
;; In the example below, (|outColor| :vec4) in fragment clause is an output spec.
;;
;; The first element of the output spec must a symbol.
;; The second element of the output spec is a keyword symbol that names a GLSL type.
;;
;; The rest elements of the clause are definitions of the glsl codes.
;; You can specify function with DEFUN form.
;; DEFUN form require FTYPE DECLAIM.
;; (VALUES) represents 'void'.
;;
;; You can check the generated vertex shader codes and fragment shader codes by
;; evaluating (vertex-shader 'hello-triangle) and
;; (fragment-shader 'hello-triangle) respectively.

(fude-gl:defshader hello-triangle 330 (fude-gl:xy)
  (:vertex ()
    (declaim (ftype (function nil (values)) main))
    (defun main () "gl_Position = vec4(xy, 0.0, 1.0);"))
  (:fragment ((|outColor| :vec4))
    (declaim (ftype (function nil (values)) main))
    (defun main () "outColor = vec4(1.0, 1.0, 1.0, 1.0);")))

;; After define shader, you can make vertex vector via cl:make-instance.
;; Making vertex via MAKE-INSTANCE may be a bit annoying for you but it is good as documentation.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *triangle*
    (concatenate '(array single-float (*))
                 (make-instance 'hello-triangle :x 0.0 :y 0.5)
                 ;; If vertex-attribute is only one, you can use it directly.
                 (make-instance 'fude-gl:xy :x 0.5 :y -0.5)
                 ;; Of course, you can use cl:vector literal.
                 ;; NOTE: The vector must be a single-float element-type.
                 ;; In this case, it is cl:concatenate above that guarantees it.
                 #(-0.5 -0.5))))

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
    (fude-gl:with-shader ())
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    ;; To clear window, WITH-CLEAR is recomended.
    ;; The first argument ((win (:color-buffer-bit)) in the example below) is bind.
    ;; The first element of the bind (win in the example below.) is a variable that is binding the window.
    ;; The second element of the bind ((:color-buffer-bit) in the example below.) is buffer-bit-spec.
    ;; Each element must be the type of fude-gl::buffer-bit.
    ;; For detail, evaluate (describe 'fude-gl::buffer-bit).
    (fude-gl:with-clear (win (:color-buffer-bit))
      ;; To draw vertices, you can use DRAW with passing vertices name.
      (fude-gl:draw 'hello-triangle))))

;;;; UNIFORM-DEMO
;;
;; In this example, we explain how to define uniform variable in the shader and
;; how to send data to the uniforms.
;;

(fude-gl:defshader uniform-demo 330 (fude-gl:xy)
  ;; Re-use vertex-shader of HELLO-TRIANGLE.
  (:vertex () 'hello-triangle)
  ;; To use uniform, you must specify uniform-spec.
  ;; Lambda-list-keyword &UNIFORM is used to specify uniform-spec in the shader-lambda-list.
  ;; The first element of the uniform-spec must be a symbol.
  ;; The second element of the uniform-spec is a keyword symbol that names GLSL type.
  (:fragment ((|outColor| :vec4) &uniform (|triangleColor| :vec3))
    (declaim (ftype (function nil (values)) main))
    (defun main () "outColor = vec4(triangleColor, 1.0);")))

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
    ;; In order to avoid inner loop alocating.
    (let ((vec (vector 0 0 0))))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      ;; Update RED of the vec.
      (setf (aref vec 0) (/ (+ 1.0 (sin (get-internal-real-time))) 2))
      ;; To send a data, you can use SETF with fude-gl:UNIFORM.
      ;; The first argument of fude-gl:uniform is a shader name.
      ;; The second argument of fude-gl:uniform is a uniform name.
      (setf (fude-gl:uniform 'uniform-demo "triangleColor") vec)
      (fude-gl:draw 'uniform-demo))))

;;;; COLORED-TRIANGLE
;; You can specify some attributes in the attributes list.

(fude-gl:defshader colored-triangle 330 (fude-gl:xy fude-gl:rgb)
  ;; NOTE: Out-spec-variable is filtered by change-case:camel-case.
  ;; The symbol `color` is not needed to be multiple-escaped. (i.e. |color|.)
  ;; You can use `out-color` instead of `|outColor|` in the fragment shader lambda list.
  ;; (Although, not recommended due to its looking is different.)
  (:vertex ((color :vec3))
    (declaim (ftype (function nil (values)) main))
    (defun main () "color = rgb;" "gl_Position = vec4(xy, 0.0, 1.0);"))
  (:fragment ((|outColor| :vec4))
    (declaim (ftype (function nil (values)) main))
    (defun main () "outColor = vec4(color, 1.0);")))

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
    (fude-gl:with-shader ())
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      (fude-gl:draw 'colored-triangle))))

;;;; ELEMENT-BUFFER
;;
;; In this example, we explain how to use element buffer.
;;

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
    (fude-gl:with-shader ())
    (let ((wireframe nil)))
    (sdl2:with-event-loop (:method :poll)
      (:keydown ()
        (if (setq wireframe (not wireframe))
            (%gl:polygon-mode :front-and-back :line)
            (%gl:polygon-mode :front-and-back :fill)))
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      (fude-gl:draw 'element-buffer))))

;;;; TEXTURE
;;
;; In this example, we explain how to use texture.
;;

(fude-gl:defshader texture-demo 330 (fude-gl:xy fude-gl:rgb fude-gl:st)
  ;; This is an experimental feature though you can use S-Expression glsl code.
  ;;
  ;; Pros:
  ;; Variable existence checking is done in lisp compile time.
  ;; Free from the semi-colon.
  ;; Free from the multiple-escaped symbol.
  ;;
  ;; Cons:
  ;; S-Expression glsl code is very lisp-friendly but of course, it is not completely the same as common-lisp.
  ;; Code reader (the third person that includes yourself who is in the future.) may confuse easily.
  (:vertex ((color :vec3) (texcoord :vec2))
    (declaim (ftype (function nil (values)) main))
    (defun main ()
      (setf texcoord fude-gl:st
            color fude-gl:rgb
            gl-position (vec4 fude-gl:xy 0.0 1.0))))
  (:fragment ((out-color :vec4) &uniform (tex :|sampler2D|))
    (declaim (ftype (function nil (values)) main))
    (defun main ()
      (setf out-color (* (texture tex texcoord) (vec4 color 1.0))))))

(defparameter *png*
  (opticl:read-png-file
    (probe-file
      (merge-pathnames "examples/lisplogo_alien_128.png"
                       (asdf:system-source-directory
                         (asdf:find-system :fude-gl-examples))))))

;; Macro DEFTEXTURE defines a texture.
;;
;; The first argument (LISP-ALIEN in the example below) must be a symbol.
;;
;; The second argument (:TEXTURE-2D in the example below) must be a texture-target.
;; For details, evaluate (describe 'fude-gl:texture-target).
;;
;; The third argument is a form to initialize texture.
;; FUDE-GL:TEX-IMAGE-2D is a thin wrapper for gl:tex-image-2d.

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
    (fude-gl:with-shader ()
      ;; To bind current texture.
      (fude-gl:in-texture 'lisp-alien))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      (fude-gl:draw 'texture-demo))))

;;;; MIX
;;
;; In this example, we explain how to handle some textures.
;;

(fude-gl:defshader mix-demo 330 (fude-gl:xy fude-gl:st)
  (:vertex ((texcoord :vec2))
    (declaim (ftype (function nil (values)) main))
    (defun main ()
      (setf texcoord fude-gl:st
            gl-position (vec4 fude-gl:xy 0.0 1.0))))
  (:fragment ((out-color :vec4) &uniform (tex1 :|sampler2D|)
              (tex2 :|sampler2D|) (weight :float))
    (declaim (ftype (function nil (values)) main))
    (defun main ()
      (setf out-color
              (mix (texture tex1 texcoord) (texture tex2 texcoord) weight)))))

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
    (fude-gl:with-shader () (fude-gl:in-texture 'lisp-logo))
    (let ((weight
           (cons (constantly 0.5)
                 (lambda () (/ (+ 1.0 (sin (get-internal-real-time))) 2))))))
    (sdl2:with-event-loop (:method :poll)
      (:keydown ()
        (rotatef (car weight) (cdr weight)))
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      ;; To set texture to :sampler2D variables,
      ;; you can use SETF with UNIFORM.
      ;; Unit location is computed automatically unless explicitly specified.
      ;;
      ;; NOTE: Unlike previous texture-demo, we does not need to fude-gl:in-texture
      ;; because underlying generic function SEND bind current texture implicitly.
      ;;
      ;; NOTE: If you feel annoying to specify keyword argument :if-does-not-exist with :create,
      ;; you can write (fude-gl:in-texture texture-name) beforehand.
      ;; In such cases, fude-gl:in-texture guarantees existence of texture instead of keyword argument.
      ;;
      ;; Case auto location detection and using if-does-not-exist.
      (setf (fude-gl:uniform 'mix-demo "tex1")
              (fude-gl:find-texture 'lisp-alien :if-does-not-exist :create))
      ;; Case specifying unit location explicitly and using fude-gl:in-texture which in with-shader above.
      (setf (fude-gl:uniform 'mix-demo "tex2" :unit 1)
              (fude-gl:find-texture 'lisp-logo))
      (setf (fude-gl:uniform 'mix-demo "weight") (funcall (car weight)))
      (fude-gl:draw 'mix-demo))))

;;;; MATRIX-OPERATIONS
;;
;; In these examples, we explain how to operate a matrix.
;; In other words, these are 3d-matrices tutorials.
;;

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
    (declaim (ftype (function nil (values)) main))
    (defun main ()
      (setf gl-position (* transform (vec4 fude-gl:xy 0.0 1.0))
            coord fude-gl:st)))
  (:fragment ((color :vec4) &uniform (tex1 :|sampler2D|) (tex2 :|sampler2D|))
    (declaim (ftype (function nil (values)) main))
    (defun main ()
      (setf color (mix (texture tex1 coord) (texture tex2 coord) 0.2)))))

(fude-gl:defvertices transform-demo
    (concatenate '(array single-float (*))
                 ;; top left
                 (make-instance 'transform-demo :x -0.5 :y 0.5 :s 0.0 :t 1.0)
                 ;; top right
                 (make-instance 'transform-demo :x 0.5 :y 0.5 :s 1.0 :t 1.0)
                 ;; bottom left
                 (make-instance 'transform-demo :x -0.5 :y -0.5 :s 0.0 :t 0.0)
                 ;; bottom right
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
    ;; FUDE-GL uses 3d-matrices as its matrix representation, but not limited.
    ;; You can use any object.
    ;; But in such cases, you need to defmethod SEND for it.
    ;;
    ;; NOTE: In order to avoid inner loop allocation.
    (let ((matrix (3d-matrices:meye 4))))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      (setf (fude-gl:uniform 'transform-demo "tex1")
              (fude-gl:find-texture 'container :if-does-not-exist :create)
            (fude-gl:uniform 'transform-demo "tex2")
              (fude-gl:find-texture 'face :if-does-not-exist :create)
            (fude-gl:uniform 'transform-demo "transform")
              (3d-matrices:nmscale
                (3d-matrices:nmrotate (fude-gl:reload matrix fude-gl:+meye4+)
                                      3d-vectors:+vz+ #.(fude-gl:radians 90))
                #.(3d-vectors:vec 0.5 0.5 0.5)))
      (fude-gl:draw 'transform-demo))))

(defun translate-x ()
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :title "Translate x"
                           :w 800
                           :h 600))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader () (fude-gl:in-vertices 'transform-demo))
    (let ((matrix (3d-matrices:meye 4)) (position (3d-vectors:vec 0 0 0))))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      ;; When set some values to the uniforms,
      ;; you can use WITH-UNIFORMS which is a family of CL:WITH-SLOTS.
      ;; Each bind is a symbol that generates a uniform glsl name
      ;; via FUDE-GL::SYMBOL-CAMEL-CASE.
      (fude-gl:with-uniforms (tex1 tex2 transform)
          'transform-demo
        (setf tex1 (fude-gl:find-texture 'container :if-does-not-exist :create)
              tex2 (fude-gl:find-texture 'face :if-does-not-exist :create)
              transform
                (progn
                 (setf (3d-vectors:vx position) (sin (get-internal-real-time)))
                 (3d-matrices:nmtranslate
                   (fude-gl:reload matrix fude-gl:+meye4+) position))))
      (fude-gl:draw 'transform-demo))))

(defun translate-y ()
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :title "Translate y"
                           :w 800
                           :h 600))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader ())
    (let ((matrix (3d-matrices:meye 4)) (position (3d-vectors:vec 0 0 0))))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      (fude-gl:with-uniforms (tex1 tex2 transform)
          'transform-demo
        (setf tex1 (fude-gl:find-texture 'container :if-does-not-exist :create)
              tex2 (fude-gl:find-texture 'face :if-does-not-exist :create)
              transform
                (progn
                 (setf (3d-vectors:vy position) (sin (get-internal-real-time)))
                 (3d-matrices:nmtranslate
                   (fude-gl:reload matrix fude-gl:+meye4+) position))))
      (fude-gl:draw 'transform-demo))))

(defun scaling ()
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :title "Scaling"
                           :w 800
                           :h 600))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader ())
    (let ((matrix (3d-matrices:meye 4)) (scale (3d-vectors:vec 0 0 0))))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      (fude-gl:with-uniforms (tex1 tex2 transform)
          'transform-demo
        (setf tex1 (fude-gl:find-texture 'container :if-does-not-exist :create)
              tex2 (fude-gl:find-texture 'face :if-does-not-exist :create)
              transform
                (3d-matrices:nmscale
                  (3d-matrices:nmtranslate
                    (fude-gl:reload matrix fude-gl:+meye4+)
                    #.(3d-vectors:vec 0 0 0))
                  (let ((v (abs (sin (get-internal-real-time)))))
                    (3d-vectors:vsetf scale v v)))))
      (fude-gl:draw 'transform-demo))))

(defun rotating ()
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :title "Rotating"
                           :w 800
                           :h 600))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader ())
    (let ((matrix (3d-matrices:meye 4))))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      (fude-gl:with-uniforms (tex1 tex2 transform)
          'transform-demo
        (setf tex1 (fude-gl:find-texture 'container :if-does-not-exist :create)
              tex2 (fude-gl:find-texture 'face :if-does-not-exist :create)
              transform
                (3d-matrices:nmrotate
                  (3d-matrices:nmtranslate
                    (fude-gl:reload matrix fude-gl:+meye4+)
                    #.(3d-vectors:vec 0 0 0))
                  3d-vectors:+vz+ (fude-gl:radians (get-internal-real-time)))))
      (fude-gl:draw 'transform-demo))))

;;;; COORD-DEMO
;;
;; In this example, we explain how to do model view projection matrix operation.
;;

(fude-gl:defshader coord-demo 330 (fude-gl:xy fude-gl:st)
  (:vertex ((coord :vec2) &uniform (model :mat4) (view :mat4)
            (projection :mat4))
    (declaim (ftype (function nil (values)) main))
    (defun main ()
      (setf gl-position (* projection view model (vec4 fude-gl:xy 0.0 1.0))
            coord fude-gl:st)))
  (:fragment ((color :vec4) &uniform (tex1 :|sampler2D|) (tex2 :|sampler2D|))
    (declaim (ftype (function nil (values)) main))
    (defun main ()
      (setf color (mix (texture tex1 coord) (texture tex2 coord) 0.2)))))

(fude-gl:defvertices coord-demo
    (concatenate '(array single-float (*))
                 ;; top left
                 (make-instance 'coord-demo :x -0.5 :y 0.5 :s 0.0 :t 1.0)
                 ;; top right
                 (make-instance 'coord-demo :x 0.5 :y 0.5 :s 1.0 :t 1.0)
                 ;; bottom left
                 (make-instance 'coord-demo :x -0.5 :y -0.5 :s 0.0 :t 0.0)
                 ;; bottom right
                 (make-instance 'coord-demo :x 0.5 :y -0.5 :s 1.0 :t 0.0))
  :indices `((0 1 2 2 3 1)))

(defun coord-demo ()
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :title "Coord demo"
                           :w 800
                           :h 600))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader ())
    (let ((m
           (3d-matrices:nmrotate (3d-matrices:meye 4) 3d-vectors:+vx+
                                 (fude-gl:radians -55)))
          (v (3d-matrices:mtranslation (3d-vectors:vec 0 0 -3)))
          (p
           (3d-matrices:mperspective 45
                                     (multiple-value-call #'/
                                       (sdl2:get-window-size win))
                                     0.1 100))))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      (fude-gl:with-uniforms (tex1 tex2 model view projection)
          'coord-demo
        (setf tex1 (fude-gl:find-texture 'container :if-does-not-exist :create)
              tex2 (fude-gl:find-texture 'face :if-does-not-exist :create)
              model m
              view v
              projection p))
      (fude-gl:draw 'coord-demo))))

;;;; ORTHO-DEMO

(fude-gl:defshader ortho-demo 330 (fude-gl:xy fude-gl:st)
  (:vertex ((coord :vec2) &uniform (projection :mat4) (model :mat4))
    (declaim (ftype (function nil (values)) main))
    (defun main ()
      (setf gl-position (* projection model (vec4 fude-gl:xy 0.0 1.0))
            coord fude-gl:st)))
  (:fragment ((color :vec4) &uniform (tex :|sampler2D|))
    (declaim (ftype (function nil (values)) main))
    (defun main () (setf color (texture tex coord)))))

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
          (fude-gl:in-vertices 'ortho-demo)
          (let ((p (3d-matrices:mortho 0 800 0 600 -1 1))
                (m (fude-gl:model-matrix 0 0 100 100)))
            (sdl2:with-event-loop (:method :poll)
              (:quit ()
                t)
              (:idle ()
                (fude-gl:with-clear (win (:color-buffer-bit))
                  (fude-gl:in-texture 'face)
                  (fude-gl:with-uniforms (model projection)
                      'ortho-demo
                    (setf model m
                          projection p))
                  (fude-gl:draw 'ortho-demo))))))))))

;;;; DEPTH-DEMO
;;
;; In this example, we explain how to do depth testing.
;;

(fude-gl:defshader depth-demo 330 (fude-gl:xyz fude-gl:st)
  (:vertex ((coord :vec2) &uniform (model :mat4) (view :mat4)
            (projection :mat4))
    (declaim (ftype (function nil (values)) main))
    (defun main ()
      (setf gl-position (* projection view model (vec4 fude-gl:xyz 1.0))
            coord fude-gl:st)))
  (:fragment ((color :vec4) &uniform (tex1 :|sampler2D|) (tex2 :|sampler2D|))
    (declaim (ftype (function nil (values)) main))
    (defun main ()
      (setf color (mix (texture tex1 coord) (texture tex2 coord) 0.2)))))

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
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :title "Depth demo"
                           :w 800
                           :h 600))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader () (gl:enable :depth-test)) ; <---
    (let ((m (3d-matrices:meye 4))
          (v (3d-matrices:mtranslation (3d-vectors:vec 0 0 -3)))
          (p
           (3d-matrices:mperspective 45
                                     (multiple-value-call #'/
                                       (sdl2:get-window-size win))
                                     0.1 100))))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit :depth-buffer-bit)) ; <---
      (fude-gl:with-uniforms (tex1 tex2 model view projection)
          'depth-demo
        (setf tex1 (fude-gl:find-texture 'container :if-does-not-exist :create)
              tex2 (fude-gl:find-texture 'face :if-does-not-exist :create)
              model
                (3d-matrices:nmrotate (fude-gl:reload m fude-gl:+meye4+)
                                      #.(3d-vectors:vec 0.5 1 0)
                                      (fude-gl:radians
                                        (get-internal-real-time)))
              view v
              projection p))
      (fude-gl:draw 'depth-demo))))

;;;; CUBES
;;
;; Here is one more depth demo.
;;

(fude-gl:defshader cubes 330 (fude-gl:xyz fude-gl:st)
  (:vertex ((coord :vec2) &uniform (model :mat4) (view :mat4)
            (projection :mat4))
    (declaim (ftype (function nil (values)) main))
    (defun main ()
      (setf gl-position (* projection view model (vec4 fude-gl:xyz 1.0))
            coord fude-gl:st)))
  (:fragment ((color :vec4) &uniform (tex1 :|sampler2D|) (tex2 :|sampler2D|))
    (declaim (ftype (function nil (values)) main))
    (defun main ()
      (setf color (mix (texture tex1 coord) (texture tex2 coord) 0.2)))))

(fude-gl:defvertices cubes *depth-demo*)

(defun cubes ()
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :title "Cubes"
                           :w 800
                           :h 600))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader ())
    (let ((cube-positions
           (list (3d-vectors:vec 0 0 0) (3d-vectors:vec 2 5 -15)
                 (3d-vectors:vec -1.5 -2.2 -2.5)
                 (3d-vectors:vec -3.8 -2.0 -12.3)
                 (3d-vectors:vec 2.4 -0.4 -3.5) (3d-vectors:vec -1.7 3 -7.5)
                 (3d-vectors:vec 1.3 -2 -2.5) (3d-vectors:vec 1.5 2 -2.5)
                 (3d-vectors:vec 1.5 0.2 -1.5) (3d-vectors:vec -1.3 1 -1.5)))
          (matrix (3d-matrices:meye 4))
          (v (3d-matrices:mtranslation (3d-vectors:vec 0 0 -3)))
          (p
           (3d-matrices:mperspective 45
                                     (multiple-value-call #'/
                                       (sdl2:get-window-size win))
                                     0.1 100)))
      (gl:enable :depth-test)) ; <--- When enable :depth-test...
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    ;; you should clear :depth-buffer-bit.
    (fude-gl:with-clear (win (:color-buffer-bit :depth-buffer-bit)) ; <---
      (fude-gl:with-uniforms (tex1 tex2 model view projection)
          'cubes
        (setf tex1 (fude-gl:find-texture 'container :if-does-not-exist :create)
              tex2 (fude-gl:find-texture 'face :if-does-not-exist :create))
        (loop :for pos :in cube-positions
              :for i :upfrom 0
              :do (setf model
                          (3d-matrices:nmrotate
                            (3d-matrices:nmtranslate
                              (fude-gl:reload matrix fude-gl:+meye4+) pos)
                            #.(3d-vectors:vec 1 0.3 0.5)
                            (fude-gl:radians (* 20 i)))
                        view v
                        projection p)
                  (fude-gl:draw 'cubes))))))

;;;; CAMERAS
;;
;; In this example, we explain how to use camera.
;;

(defun cameras ()
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :title "Cameras"
                           :w 800
                           :h 600))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader ())
    (let ((cube-positions
           (list (3d-vectors:vec 0 0 0) (3d-vectors:vec 2 5 -15)
                 (3d-vectors:vec -1.5 -2.2 -2.5)
                 (3d-vectors:vec -3.8 -2.0 -12.3)
                 (3d-vectors:vec 2.4 -0.4 -3.5) (3d-vectors:vec -1.7 3 -7.5)
                 (3d-vectors:vec 1.3 -2 -2.5) (3d-vectors:vec 1.5 2 -2.5)
                 (3d-vectors:vec 1.5 0.2 -1.5) (3d-vectors:vec -1.3 1 -1.5)))
          ;; To make camera, you can use a function MAKE-CAMERA.
          ;; For detail, evaluate (describe 'fude-gl:make-camera).
          (camera (fude-gl:make-camera))
          (matrix (3d-matrices:meye 4))
          (p
           (3d-matrices:mperspective 45
                                     (multiple-value-call #'/
                                       (sdl2:get-window-size win))
                                     0.1 100)))
      (gl:enable :depth-test))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit :depth-buffer-bit))
      (fude-gl:with-uniforms (tex1 tex2 model view projection)
          'cubes
        (setf tex1 (fude-gl:find-texture 'container :if-does-not-exist :create)
              tex2 (fude-gl:find-texture 'face :if-does-not-exist :create))
        (let* ((radius 10)
               ;; To move camera you can use a function MOVE.
               ;; The first argument is a camera object.
               ;; The rest arguments are new X, Y and Z.
               (moved
                (fude-gl:move camera (* (sin (get-internal-real-time)) radius)
                              0 (* (cos (get-internal-real-time)) radius)))
               ;; To get a view matrix, you can use a function VIEW.
               ;; The first argument is a camera object.
               ;; The keyword parameter :TARGET specifies to look at CAMERA-TARGET.
               (v (fude-gl:view moved :target t)))
          (loop :for pos :in cube-positions
                :for i :upfrom 0
                :do (setf model
                            (3d-matrices:nmrotate
                              (3d-matrices:nmtranslate
                                (fude-gl:reload matrix fude-gl:+meye4+) pos)
                              #.(3d-vectors:vec 1 0.3 0.5)
                              (fude-gl:radians (* 20 i)))
                          view v
                          projection p)
                    (fude-gl:draw 'cubes)))))))

;;;; WALK-AROUND
;;
;; One more camera example with handling cursor input.
;;

(defparameter *cube-positions*
  (list (3d-vectors:vec 0 0 0) (3d-vectors:vec 2 5 -15)
        (3d-vectors:vec -1.5 -2.2 -2.5) (3d-vectors:vec -3.8 -2.0 -12.3)
        (3d-vectors:vec 2.4 -0.4 -3.5) (3d-vectors:vec -1.7 3 -7.5)
        (3d-vectors:vec 1.3 -2 -2.5) (3d-vectors:vec 1.5 2 -2.5)
        (3d-vectors:vec 1.5 0.2 -1.5) (3d-vectors:vec -1.3 1 -1.5)))

(defun move-camera
       (keysym camera &optional (delta internal-time-units-per-second))
  (let ((camera-speed (* 2.5 (/ delta internal-time-units-per-second))))
    (case (sdl2:scancode keysym)
      ;; To modify CAMERA-POSITION,
      ;; you should use 3D-VECTORS's N prefixed functions.
      ;; I.e. destructively modify it.
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
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :title "Walk around"
                           :w 800
                           :h 600))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader ())
    (let ((camera (fude-gl:make-camera))
          (matrix (3d-matrices:meye 4))
          (p
           (3d-matrices:mperspective 45
                                     (multiple-value-call #'/
                                       (sdl2:get-window-size win))
                                     0.1 100))
          ;; In order to manage delta time.
          (time (fude-gl:make-delta-time)))
      (gl:enable :depth-test))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t)
      (:keydown (:keysym keysym)
        (move-camera keysym camera
                     ;; delta time.
                     fude-gl:*delta*)))
    (:idle nil)
    ;; Automatically update fude-gl:*delta* time.
    (fude-gl::with-delta-time (time))
    (fude-gl:with-clear (win (:color-buffer-bit :depth-buffer-bit))
      (fude-gl:with-uniforms (tex1 tex2 model view projection)
          'cubes
        (setf tex1 (fude-gl:find-texture 'container :if-does-not-exist :create)
              tex2 (fude-gl:find-texture 'face :if-does-not-exist :create))
        (loop :for pos :in *cube-positions*
              :for i :upfrom 0
              :do (setf model
                          (3d-matrices:nmrotate
                            (3d-matrices:nmtranslate
                              (fude-gl:reload matrix fude-gl:+meye4+) pos)
                            #.(3d-vectors:vec 1 0.3 0.5)
                            (fude-gl:radians (* 20 i)))
                        view (fude-gl:view camera)
                        projection p)
                  (fude-gl:draw 'cubes))))))

;;;; LOOK-AROUND

(defun look-around ()
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :title "Look around"
                           :w 800
                           :h 600))
    (sdl2:with-gl-context (context win)
      (sdl2:hide-cursor))
    (fude-gl:with-shader ())
    (let* ((camera
            ;; Eular angles supported camera.
            (progn
             (multiple-value-bind (w h)
                 (sdl2:get-window-size win)
               (sdl2:warp-mouse-in-window win (floor w 2) (floor h 2)))
             (multiple-value-bind (x y mask)
                 (sdl2:get-global-mouse-state)
               (declare (ignore mask))
               (fude-gl:make-first-person :last-position (3d-vectors:vec3 x y
                                                                          0)))))
           (matrix (3d-matrices:meye 4))
           (p
            (3d-matrices:mperspective 45
                                      (multiple-value-call #'/
                                        (sdl2:get-window-size win))
                                      0.1 100))
           (time (fude-gl:make-delta-time)))
      (gl:enable :depth-test))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t)
      (:keydown (:keysym keysym)
        (move-camera keysym camera fude-gl:*delta*)))
    (:idle nil)
    (fude-gl:with-delta-time (time))
    (fude-gl:with-clear (win (:color-buffer-bit :depth-buffer-bit))
      (fude-gl:with-uniforms (tex1 tex2 model view projection)
          'cubes
        (setf tex1 (fude-gl:find-texture 'container :if-does-not-exist :create)
              tex2 (fude-gl:find-texture 'face :if-does-not-exist :create))
        (loop :with camera-view
                    := (fude-gl:view ;; To update camera view.
                                     (multiple-value-call #'fude-gl:lookat
                                       camera
                                       (sdl2:get-global-mouse-state)))
              :for pos :in *cube-positions*
              :for i :upfrom 0
              :do (setf model
                          (3d-matrices:nmrotate
                            (3d-matrices:nmtranslate
                              (fude-gl:reload matrix fude-gl:+meye4+) pos)
                            #.(3d-vectors:vec 1 0.3 0.5)
                            (fude-gl:radians (* 20 i)))
                        view camera-view
                        projection p)
                  (fude-gl:draw 'cubes))))))
      (:keydown (:keysym keysym)
        (move-camera keysym camera)))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit :depth-buffer-bit))
      (multiple-value-call #'update-camera-front
        camera
        (sdl2:get-global-mouse-state))
      (fude-gl:with-uniforms (tex1 tex2 model view projection)
          'cubes
        (setf tex1 (fude-gl:find-texture 'container :if-does-not-exist :create)
              tex2 (fude-gl:find-texture 'face :if-does-not-exist :create))
        (loop :for pos :in *cube-positions*
              :for i :upfrom 0
              :do (setf model
                          (3d-matrices:nmrotate
                            (3d-matrices:nmtranslate
                              (fude-gl:reload matrix fude-gl:+meye4+) pos)
                            #.(3d-vectors:vec 1 0.3 0.5)
                            (fude-gl:radians (* 20 i)))
                        view (fude-gl:view camera)
                        projection p)
                  (fude-gl:draw 'cubes))))))

;;;; FONT
;;
;; In this example, we explain how to render text.
;;

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
    ;; To render text, you can use a macro WITH-TEXT instead of WITH-SHADER.
    ;; More correctly, WITH-TEXT is expanded to WITH-SHADER form.
    ;; The first argument is spec.
    ;; The first element of the spec must be a symbol that is evaluated to sdl2-ffi:sdl-window.
    ;; The keyword parameter :SIZE specifies the font size.
    (fude-gl:with-text (win :size 32))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t)
      (:idle ()
        (fude-gl:with-clear (win (:color-buffer-bit))
          ;; The first argument of the text renderer is a string to render.
          ;; The keyword parameter :X and :Y specifies position.
          ;; You can use :CENTER to specify centering the text.
          ;; In such cases, you must specify keyword argument :WIN.
          ;; In other words, if you do not use :center you do not need to specify :WIN.
          (fude-gl:render-text
            (multiple-value-call #'format
              nil
              "Mouse: X=~D Y=~D Mask=~S"
              (sdl2:mouse-state))
            :x 0
            :y :center
            :win win))))))

;;;; INSTANCING
;;
;; In this example, we explain how to use instancing with array uniform.
;;

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
  ;; (e.g. 100 in the :vertex clause below.)
  (:vertex ((f-color :vec3) &uniform (offsets :vec2 100))
    (declaim (ftype (function nil (values)) main))
    ;; With S-Expression glsl, LET needs type-spec in the second element of the bind.
    (defun main ()
      (let ((offset :vec2 (aref offsets |gl_InstanceID|)))
        (setf gl-position (vec4 (+ fude-gl:xy offset) 0.0 1.0)
              f-color fude-gl:rgb))))
  (:fragment ((frag-color :vec4))
    (declaim (ftype (function nil (values)) main))
    (defun main () (setf frag-color (vec4 f-color 1.0)))))

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
            :do (setf (fude-gl:uniform 'instancing
                                       (format nil "offsets[~A]" i))
                        vec2)))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t)
      (:idle ()
        (fude-gl:with-clear (win (:color-buffer-bit))
          ;; NOTE: Could not use fude-gl:draw
          ;; due to instancing is not INSTANCED-VERTICES class.
          ;; How to make INSTANCED-VERTICES class, see next demo.
          (%gl:draw-arrays-instanced :triangles 0 6 (length translations)))))))

;;;; INSTANCED-ARRAYS
;;
;; In this example, we explain how to use instancing with attribute.
;;

(fude-gl:defshader instanced-arrays-demo 330 (fude-gl:xy fude-gl:rgb
                                              fude-gl:offset) ; <---
  (:vertex ((f-color :vec3))
    (declaim (ftype (function nil (values)) main))
    (defun main ()
      (setf gl-position (vec4 (+ fude-gl:xy fude-gl:offset) 0.0 1.0)
            f-color fude-gl:rgb)))
  (:fragment ((frag-color :vec4))
    (declaim (ftype (function nil (values)) main))
    (defun main () (setf frag-color (vec4 f-color 1.0)))))

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
  ;; To link instanced-array with attribute.
  ;; The value is attribute and array pair alist.
  :instances `((fude-gl:offset ,*translations*)))

(defun instanced-arrays-demo ()
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :w 800
                           :h 600
                           :title "Instanced arrays demo"))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader ())
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      (fude-gl:draw 'instanced-arrays-demo))))

;;;; INSTANCE-ID-DEMO
;;
;; One more example to show how instance ID works.
;;

(fude-gl:defshader instance-id-demo 330 (fude-gl:xy fude-gl:rgb fude-gl:offset)
  (:vertex ((f-color :vec3))
    (declaim (ftype (function nil (values)) main))
    ;; |gl_InstanceID| or gl-instance-i-d, use which you like.
    (defun main ()
      (setf gl-position
              (vec4 (+ (* fude-gl:xy (/ |gl_InstanceID| 100.0)) fude-gl:offset)
               0.0 1.0)
            f-color fude-gl:rgb)))
  (:fragment ((frag-color :vec4))
    (declaim (ftype (function nil (values)) main))
    (defun main () (setf frag-color (vec4 f-color 1.0)))))

(fude-gl:defvertices instance-id-demo *instancing*
  :instances `((fude-gl:offset ,*translations*)))

(defun instance-id-demo ()
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :w 800
                           :h 600
                           :title "InstanceID demo"))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader ())
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      (fude-gl:draw 'instance-id-demo))))

;;;; SOME-INSTANCES-DEMO
;;
;; In this example, we explain how to handle some instanced-array.
;;

(fude-gl:defshader some-instances-demo 330 (fude-gl:xy fude-gl:rgb
                                            fude-gl:offset fude-gl:a)
  (:vertex ((f-color :vec4))
    (declaim (ftype (function nil (values)) main))
    (defun main ()
      (setf gl-position (vec4 (+ fude-gl:xy fude-gl:offset) 0.0 1.0)
            f-color (vec4 fude-gl:rgb fude-gl:a))))
  (:fragment ((frag-color :vec4))
    (declaim (ftype (function nil (values)) main))
    (defun main () (setf frag-color f-color))))

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
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :w 800
                           :h 600
                           :title "Some instance demo"))
    (sdl2:with-gl-context (context win)
      (gl:enable :blend)
      ;; To enable alpha blending.
      (gl:blend-func :src-alpha :one-minus-src-alpha))
    (fude-gl:with-shader ())
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      (fude-gl:draw 'some-instances-demo))))

;;;; SOME-INSTANCE-DYNAMICS
;;
;; In this example, we explain how to handle instanced array dynamically.
;;

(fude-gl:defvertices some-instance-dynamics *instancing*
  :shader 'some-instances-demo
  :instances `((fude-gl:offset ,*translations*)
               (fude-gl:a
                ,(make-array (array-dimension *translations* 0)
                             :element-type 'single-float
                             :initial-element 0.0)
                ;; To specify buffer usage as dynamic.
                :usage :dynamic-draw)))

(defun some-instance-dynamics ()
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :w 800
                           :h 600
                           :title "Some instance dynamic"))
    (sdl2:with-gl-context (context win)
      (gl:enable :blend)
      (gl:blend-func :src-alpha :one-minus-src-alpha))
    (fude-gl:with-shader ())
    (let ((vec
           ;; To get gl-array object.
           (fude-gl:buffer-source ;; To get buffer object.
                                  ;; The first argument is a vertices name.
                                  ;; The second argument is an attribute name.
                                  (fude-gl:instances-buffer
                                    'some-instance-dynamics 'fude-gl:a)))))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      ;; Destructively modify buffer source.
      (setf (gl:glaref vec (random (gl::gl-array-size vec)))
              (sin (get-internal-real-time)))
      ;; Send buffer source to GL side buffer.
      (fude-gl:send 'fude-gl:a 'some-instance-dynamics)
      (fude-gl:draw 'some-instance-dynamics))))

;;;; DEPTH-TESTING

(fude-gl:defshader depth-testing 330 (fude-gl:xyz fude-gl:st)
  (:vertex ((coord :vec2) &uniform (model :mat4) (view :mat4)
            (projection :mat4))
    (declaim (ftype (function nil (values)) main))
    (defun main ()
      (setf coord fude-gl:st
            gl-position (* projection view model (vec4 fude-gl:xyz 1.0)))))
  (:fragment ((color :vec4) &uniform (tex :|sampler2D|))
    (declaim (ftype (function nil (values)) main))
    (defun main () (setf color (texture tex coord)))))

(defparameter *marble*
  (let ((pathname (merge-pathnames "marble.jpg" (user-homedir-pathname))))
    (unless (probe-file pathname)
      (dex:fetch
        "https://raw.githubusercontent.com/JoeyDeVries/LearnOpenGL/master/resources/textures/marble.jpg"
        pathname))
    (opticl:read-jpeg-file pathname)))

(fude-gl:deftexture cube-texture :texture-2d (fude-gl:tex-image-2d *marble*))

(defparameter *metal*
  (let ((pathname (merge-pathnames "metal.png" (user-homedir-pathname))))
    (unless (probe-file pathname)
      (dex:fetch
        "https://raw.githubusercontent.com/JoeyDeVries/LearnOpenGL/master/resources/textures/metal.png"
        pathname))
    (opticl:read-png-file pathname)))

(fude-gl:deftexture metal :texture-2d (fude-gl:tex-image-2d *metal*))

(fude-gl:defvertices depth-testing-cubes *depth-demo* :shader 'depth-testing)

(fude-gl:defvertices depth-testing-plane
    (coerce
      #(5.0 -0.5 5.0 2.0 0.0 ; first
        -5.0 -0.5 5.0 0.0 0.0 ; second
        -5.0 -0.5 -5.0 0.0 2.0 ; third
        5.0 -0.5 5.0 2.0 0.0 ; fourth
        -5.0 -0.5 -5.0 0.0 2.0 ; fifth
        5.0 -0.5 -5.0 2.0 2.0) ; sixth
      '(array single-float (*)))
  :shader 'depth-testing)

(defun depth-testing ()
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :w 800
                           :h 600
                           :title "Depth testing"))
    (sdl2:with-gl-context (context win)
      (gl:enable :depth-test)
      (gl:depth-func :less)) ; <---
    (fude-gl:with-shader ())
    (let* ((camera (fude-gl:make-camera))
           (matrix (3d-matrices:meye 4))
           (v (fude-gl:view camera))
           (p
            (3d-matrices:mperspective 60
                                      (multiple-value-call #'/
                                        (sdl2:get-window-size win))
                                      0.1 100)))
      (fude-gl:with-uniforms (view projection)
          'depth-testing
        (setf view v
              projection p)))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit :depth-buffer-bit))
      (fude-gl:with-uniforms (model tex)
          'depth-testing
        (fude-gl:in-vertices 'depth-testing-cubes)
        (setf tex
                (fude-gl:find-texture 'cube-texture :if-does-not-exist :create)
              model
                (3d-matrices:nmtranslate
                  (fude-gl:reload matrix fude-gl:+meye4+)
                  #.(3d-vectors:vec3 -1 0 -1)))
        (fude-gl:draw 'depth-testing-cubes)
        (setf model
                (3d-matrices:nmtranslate
                  (fude-gl:reload matrix fude-gl:+meye4+)
                  #.(3d-vectors:vec3 2 0 0)))
        (fude-gl:draw 'depth-testing-cubes)
        (fude-gl:in-vertices 'depth-testing-plane)
        (setf tex (fude-gl:find-texture 'metal :if-does-not-exist :create)
              model (fude-gl:reload matrix fude-gl:+meye4+))
        (fude-gl:draw 'depth-testing-plane)))))

;;;; FRAMEBUFFER

(fude-gl:defshader framebuffer-screen 330 (fude-gl:xy fude-gl:st)
  (:vertex ((coord :vec2))
    (declaim (ftype (function nil (values)) main))
    (defun main ()
      (setf gl-position (vec4 fude-gl:xy 0.0 1.0)
            coord fude-gl:st)))
  (:fragment ((color :vec4) &uniform (screen :|sampler2D|))
    (declaim (ftype (function nil (values)) main))
    (defun main () (setf color (vec4 (rgb (texture screen coord)) 1.0)))))

(fude-gl:defvertices framebuffer-quad
    (concatenate '(array single-float (*)) #(-1.0 1.0 0.0 1.0)
                 #(-1.0 -1.0 0.0 0.0) #(1.0 -1.0 1.0 0.0) #(-1.0 1.0 0.0 1.0)
                 #(1.0 -1.0 1.0 0.0) #(1.0 1.0 1.0 1.0))
  :shader 'framebuffer-screen)

(fude-gl:defshader framebuffer-vertices 330 (fude-gl:xyz fude-gl:st)
  (:vertex ((coord :vec2) &uniform (model :mat4) (view :mat4)
            (projection :mat4))
    (declaim (ftype (function nil (values)) main))
    (defun main ()
      (setf gl-position (* projection view model (vec4 fude-gl:xyz 1.0))
            coord fude-gl:st)))
  (:fragment ((color :vec4) &uniform (tex :|sampler2D|))
    (declaim (ftype (function nil (values)) main))
    (defun main () (setf color (texture tex coord)))))

(fude-gl:defvertices fb-cube *depth-demo* :shader 'framebuffer-vertices)

(fude-gl:defvertices plane-vertices
    (concatenate '(array single-float (*)) #(5.0 -0.5 5.0 2.0 0.0)
                 #(-5.0 -0.5 5.0 0.0 0.0) #(-5.0 -0.5 -5.0 0.0 2.0)
                 #(5.0 -0.5 5.0 2.0 0.0) #(-5.0 -0.5 -5.0 0.0 2.0)
                 #(5.0 -0.5 -5.0 2.0 2.0))
  :shader 'framebuffer-vertices)

;; Macro DEFRAMEBUF defines framebuffer.
;;
;; The first argument (STEP1 in the example below) must be a symbol names framebuffer.

(fude-gl:deframebuf step1 :width 800 :height 600)

(defun framebuffer-step1 ()
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :w 800
                           :h 600
                           :title "Frame buffer Step1"))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader () (gl:enable :depth-test))
    (let* ((camera (fude-gl:make-camera))
           (matrix (3d-matrices:meye 4))
           (view (fude-gl:view camera))
           (projection (3d-matrices:mperspective 45 (/ 800 600) 0.1 100)))
      (setf (fude-gl:uniform 'framebuffer-screen "screen") 0))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      ;; bind to framebuffer and draw scene as we normally would to color texture
      ;; For cleanup, macro WITH-FRAMEBUFFER is recommended.
      (fude-gl:with-framebuffer (step1 (:color-buffer-bit :depth-buffer-bit)
                                       :color '(0.1 0.1 0.1 1) :win win)
        (gl:enable :depth-test)
        ;; You can specify alias for uniform name.
        ;; In the example below,
        ;; specify alias 'v' for uniform "view" and
        ;; alias 'p' for uniform "projection".
        ;; When a true name for uniform is a string like `"view"` in the example,
        ;; it is used as uniform name.
        ;; When a true name for uniform is a symbol like `projection` in the example,
        ;; FUDE-GL::SYMBOL-CAMEL-CASEd string is used for uniform name.
        (fude-gl:with-uniforms (tex (v "view") (p projection) model)
            'framebuffer-vertices
          ;;; Cubes
          (setf tex
                  (fude-gl:find-texture 'container :if-does-not-exist :create)
                v view
                p projection
                model
                  (3d-matrices:nmtranslate
                    (fude-gl:reload matrix fude-gl:+meye4+)
                    #.(3d-vectors:vec3 -1 0 -1)))
          (fude-gl:draw 'fb-cube)
          (setf model
                  (3d-matrices:nmtranslate
                    (fude-gl:reload matrix fude-gl:+meye4+)
                    #.(3d-vectors:vec3 2 0 0)))
          (fude-gl:draw 'fb-cube)
          ;;; floor
          (fude-gl:in-vertices 'plane-vertices)
          (setf tex (fude-gl:find-texture 'metal :if-does-not-exist :create)
                model (fude-gl:reload matrix fude-gl:+meye4+))
          (fude-gl:draw 'plane-vertices)))
      ;; draw a quad plane with the attached framebuffer color texture
      ;; disable depth test so screen-space quad isn't discarded due to depth test.
      (gl:disable :depth-test)
      (fude-gl:in-vertices 'framebuffer-quad)
      (fude-gl:in-texture (fude-gl:framebuffer-texture 'step1))
      (fude-gl:draw 'framebuffer-quad))))

;;;; DEPTH-MAP-DEMO

(fude-gl:deframebuf depth-map
  :width 1024
  :height 1024
  ;; To specify texture format. This must be BASE-INTERNAL-FORMAT.
  ;; The default is :RGB.
  :format :depth-component
  ;; To specify texture pixel type. This must be PIXEL-TYPE.
  ;; The default is :UNSIGNED-BYTE.
  :pixel-type :float
  ;; To specify texture params. This must be TEXTURE-PNAME and its value plist.
  ;; The default is '(:texture-min-filter :linear :texture-mag-filter :linear)
  :options `(:texture-min-filter :nearest :texture-mag-filter :nearest
             :texture-wrap-s :repeat :texture-wrap-t :repeat)
  ;; To specify framebuffer attachment. This must be ATTACHMENT.
  ;; The default is :COLOR-ATTACHMENT0.
  :attachment :depth-attachment
  ;; To specify renderbuffer initializer.
  ;; This must be function as (function (framebuffer)).
  ;; The default is #'DEFAULT-RENDERBUFFER-INITIALIZER
  :renderbuffer-initializer
  (lambda (this)
    (declare (ignore this))
    (gl:draw-buffer :none)
    (gl:read-buffer :none)))

(fude-gl:defshader simple-depth 330 (fude-gl:xyz)
  (:vertex (&uniform (light-space-matrix :mat4) (model :mat4))
    (declaim (ftype (function nil (values)) main))
    (defun main ()
      (setf gl-position (* light-space-matrix model (vec4 fude-gl:xyz 1.0)))))
  (:fragment () (declaim (ftype (function nil (values)) main)) (defun main ())))

(fude-gl:defshader debug-quad 330 (fude-gl:xyz fude-gl:st)
  (:vertex ((coord :vec2))
    (declaim (ftype (function nil (values)) main))
    (defun main ()
      (setf coord fude-gl:st
            gl-position (vec4 fude-gl:xyz 1.0))))
  (:fragment ((color :vec4) &uniform (depth-map :|sampler2D|)
              (near-plane :float) (far-plane :float))
    ;; You can specify local function for shader with defun clause.
    ;; NOTE: DECLAIM is required.
    (declaim (ftype (function (:float) :float) linearize-depth))
    (defun linearize-depth (depth)
      (let ((z :float (- (* depth 2.0) 1.0))) ; Back to NDC.
        (return
         (/ (* 2.0 near-plane far-plane)
            (- (+ far-plane near-plane) (* z (- far-plane near-plane)))))))
    (declaim (ftype (function nil (values)) main))
    (defun main ()
      (let ((depth-value :float (r (texture depth-map coord))))
        ;; Perspective
        ;; (setf color (vec4 (vec3 (/ (linearize-depth depth-value) far-plane)) 1.0))
        ;; orthographic.
        (setf color (vec4 (vec3 depth-value) 1.0))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *framebuffer-plane-vertices*
    (coerce ;; positions            ;; normals         ;; texcoords
            #(25.0 -0.5 25.0 0.0 1.0 0.0 25.0 0.0 ; first
              -25.0 -0.5 25.0 0.0 1.0 0.0 0.0 0.0 ; second
              -25.0 -0.5 -25.0 0.0 1.0 0.0 0.0 25.0 ; third
              25.0 -0.5 25.0 0.0 1.0 0.0 25.0 0.0 ; fourth
              -25.0 -0.5 -25.0 0.0 1.0 0.0 0.0 25.0 ; fifth
              25.0 -0.5 -25.0 0.0 1.0 0.0 25.0 10.0) ; sixth
            '(array single-float (*)))))

(fude-gl:defvertices plane-vao *framebuffer-plane-vertices*
  :shader 'simple-depth
  :attributes '(fude-gl:xyz fude-gl:rgb fude-gl:st))

(fude-gl:defvertices shadow-cube
    (coerce ;; back face
            #(-1.0 -1.0 -1.0 0.0 0.0 -1.0 0.0 0.0 ; bottom-left
              1.0 1.0 -1.0 0.0 0.0 -1.0 1.0 1.0 ; top-right
              1.0 -1.0 -1.0 0.0 0.0 -1.0 1.0 0.0 ; bottom-right
              1.0 1.0 -1.0 0.0 0.0 -1.0 1.0 1.0 ; top-right
              -1.0 -1.0 -1.0 0.0 0.0 -1.0 0.0 0.0 ; bottom-left
              -1.0 1.0 -1.0 0.0 0.0 -1.0 0.0 1.0 ; top-left
              ;; front face
              -1.0 -1.0 1.0 0.0 0.0 1.0 0.0 0.0 ; bottom-left
              1.0 -1.0 1.0 0.0 0.0 1.0 1.0 0.0 ; bottom-right
              1.0 1.0 1.0 0.0 0.0 1.0 1.0 1.0 ; top-right
              1.0 1.0 1.0 0.0 0.0 1.0 1.0 1.0 ; top-right
              -1.0 1.0 1.0 0.0 0.0 1.0 0.0 1.0 ; top-left
              -1.0 -1.0 1.0 0.0 0.0 1.0 0.0 0.0 ; bottom-left
              ;; left face
              -1.0 1.0 1.0 -1.0 0.0 0.0 1.0 0.0 ; top-right
              -1.0 1.0 -1.0 -1.0 0.0 0.0 1.0 1.0 ; top-left
              -1.0 -1.0 -1.0 -1.0 0.0 0.0 0.0 1.0 ; bottom-left
              -1.0 -1.0 -1.0 -1.0 0.0 0.0 0.0 1.0 ; bottom-left
              -1.0 -1.0 1.0 -1.0 0.0 0.0 0.0 0.0 ; bottom-right
              -1.0 1.0 1.0 -1.0 0.0 0.0 1.0 0.0 ; top-right
              ;; right face
              1.0 1.0 1.0 1.0 0.0 0.0 1.0 0.0 ; top-left
              1.0 -1.0 -1.0 1.0 0.0 0.0 0.0 1.0 ; bottom-right
              1.0 1.0 -1.0 1.0 0.0 0.0 1.0 1.0 ; top-right
              1.0 -1.0 -1.0 1.0 0.0 0.0 0.0 1.0 ; bottom-right
              1.0 1.0 1.0 1.0 0.0 0.0 1.0 0.0 ; top-left
              1.0 -1.0 1.0 1.0 0.0 0.0 0.0 0.0 ; bottom-left
              ;; bottom face
              -1.0 -1.0 -1.0 0.0 -1.0 0.0 0.0 1.0 ; top-right
              1.0 -1.0 -1.0 0.0 -1.0 0.0 1.0 1.0 ; top-left
              1.0 -1.0 1.0 0.0 -1.0 0.0 1.0 0.0 ; bottom-left
              1.0 -1.0 1.0 0.0 -1.0 0.0 1.0 0.0 ; bottom-left
              -1.0 -1.0 1.0 0.0 -1.0 0.0 0.0 0.0 ; bottom-right
              -1.0 -1.0 -1.0 0.0 -1.0 0.0 0.0 1.0 ; top-right
              ;; top face
              -1.0 1.0 -1.0 0.0 1.0 0.0 0.0 1.0 ; top-left
              1.0 1.0 1.0 0.0 1.0 0.0 1.0 0.0 ; bottom-right
              1.0 1.0 -1.0 0.0 1.0 0.0 1.0 1.0 ; top-right
              1.0 1.0 1.0 0.0 1.0 0.0 1.0 0.0 ; bottom-right
              -1.0 1.0 -1.0 0.0 1.0 0.0 0.0 1.0 ; top-left
              -1.0 1.0 1.0 0.0 1.0 0.0 0.0 0.0 ; bottom-left
              )
            '(array single-float (*)))
  :shader 'simple-depth
  ;; When shader spec differents actual vertices spec,
  ;; specify attributes.
  :attributes '(fude-gl:xyz fude-gl:rgb fude-gl:st))

(fude-gl:defvertices shadow-quad
    (coerce ;; positions        // texture Coords
            #(-1.0 1.0 0.0 0.0 1.0 ; first
              -1.0 -1.0 0.0 0.0 0.0 ; second
              1.0 1.0 0.0 1.0 1.0 ; third
              1.0 -1.0 0.0 1.0 0.0 ; fourth
              )
            '(array single-float (*)))
  :shader 'debug-quad
  ;; To specify draw mode. The default is :TRIANGLES.
  :draw-mode
  :triangle-strip)

(fude-gl:deftexture wood :texture-2d
  (fude-gl:tex-image-2d
    (let ((pathname (merge-pathnames "wood.png" (user-homedir-pathname))))
      (unless (probe-file pathname)
        (dex:fetch
          "https://raw.githubusercontent.com/JoeyDeVries/LearnOpenGL/master/resources/textures/wood.png"
          pathname))
      (opticl:read-png-file pathname))))

(defun framebuffer-shadow ()
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :w 800
                           :h 600
                           :title "Framebuffer depth-map"))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader () (gl:enable :depth-test))
    (let* ((light-position (3d-vectors:vec3 -2 4 -1))
           (near-plane 1.0) ; When projection is perspective,
           (far-plane 7.5) ; these vars will be refered.
           (ortho-origin
            (3d-matrices:mortho -10 10 -10 10 near-plane far-plane))
           (ortho (3d-matrices:mortho -10 10 -10 10 near-plane far-plane))
           (lookat
            (3d-matrices:mlookat light-position (3d-vectors:vec3 0 0 0)
                                 (3d-vectors:vec3 0 1 0)))))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil
     (fude-gl:with-clear (win (:color-buffer-bit :depth-buffer-bit)
                              :color '(0.1 0.1 0.1 1))
       ;; 1. render depth of scene to texture (from light's perspective)
       ;; render scene from light's point of view
       (fude-gl:with-framebuffer (depth-map (:depth-buffer-bit)
                                            ;; To specify not clear color.
                                            :color nil :win win)
         (setf (fude-gl:uniform 'simple-depth "lightSpaceMatrix")
                 (3d-matrices:nm* (fude-gl:reload ortho ortho-origin) lookat))
         (render-scene 'plane-vao))
       (gl:clear :color-buffer-bit :depth-buffer-bit)
       ;; render Depth map to quad for visual debugging
       (fude-gl:with-uniforms ((np "nearPlane") (fp "farPlane")
                               (depth-map :unit 0))
         ;; To explicitly represents the relationship of the
         ;; vertices and shader.
         (fude-gl:shader 'shadow-quad)
         (setf ; Comment outted, only need with perspective projection.
                 #|
               np near-plane
               fp far-plane ; |#
               depth-map (fude-gl:framebuffer-texture 'depth-map))
         (fude-gl:draw 'shadow-quad))))))

(defun render-scene (vertices)
  ;; floor
  (fude-gl:in-vertices vertices)
  (let ((shader (fude-gl:shader vertices)) (m (3d-matrices:meye 4)))
    (fude-gl:with-uniforms (model)
        shader
      (setf model (fude-gl:reload m fude-gl:+meye4+))
      (fude-gl:draw vertices)
      (let ((m (fude-gl:reload m fude-gl:+meye4+)))
        (3d-matrices:nmtranslate m #.(3d-vectors:vec3 0 1.5 0))
        (3d-matrices:nmscale m #.(3d-vectors:vec3 0.5 0.5 0.5))
        (setf model m)
        (fude-gl:draw 'shadow-cube))
      (let ((m (fude-gl:reload m fude-gl:+meye4+)))
        (3d-matrices:nmtranslate m #.(3d-vectors:vec3 2 0 1))
        (3d-matrices:nmscale m #.(3d-vectors:vec3 0.5 0.5 0.5))
        (setf model m)
        (fude-gl:draw 'shadow-cube))
      (let ((m (fude-gl:reload m fude-gl:+meye4+)))
        (3d-matrices:nmtranslate m #.(3d-vectors:vec3 -1 0 2))
        (3d-matrices:nmrotate m #.(3d-vectors:vunit (3d-vectors:vec3 1 0 1))
                              #.(fude-gl:radians 60))
        (3d-matrices:nmscale m #.(3d-vectors:vec3 0.25 0.25 0.25))
        (setf model m)
        (fude-gl:draw 'shadow-cube)))))

;;;; SHADOW

(fude-gl:define-vertex-attribute normal (a b c))

(fude-gl:defshader shadow-mapping 330 (fude-gl:xyz normal fude-gl:st)
  ;; To specify user defined complex type (ARGSET in the example below),
  ;; var-spec list is used for second element of var-spec.
  (:vertex ((argset
             ((frag-pos :vec3) (normal :vec3) (coord :vec2)
              (frag-pos-light-space :vec4)))
            &uniform (projection :mat4) (view :mat4) (model :mat4)
            (light-space-matrix :mat4))
    (declaim (ftype (function nil (values)) main))
    (defun main ()
      (with-slots (frag-pos (n normal) coord frag-pos-light-space)
          argset
        (setf frag-pos (vec3 (* model (vec4 fude-gl:xyz 1.0)))
              n (* (transpose (inverse (mat3 model))) normal)
              coord fude-gl:st
              frag-pos-light-space (* light-space-matrix (vec4 frag-pos 1.0))
              gl-position (* projection view model (vec4 fude-gl:xyz 1.0))))))
  (:fragment ((out-color :vec4) &uniform (diffuse-texture :|sampler2D|)
              (shadow-map :|sampler2D|) (light-pos :vec3) (view-pos :vec3))
    (declaim (ftype (function (:vec4) float) calculate-shadow))
    (defun calculate-shadow (frag-pos-light-space)
      (let ((proj-coords
             :vec3
             (+ 0.5
                (* 0.5
                   (/ (xyz frag-pos-light-space) (w frag-pos-light-space))))))
        (return
         (if (< (r (texture shadow-map (xy proj-coords))) (z proj-coords))
             1.0
             0.0))))
    (declaim (ftype (function nil (values)) main))
    (defun main ()
      (with-slots (coord (n normal) frag-pos frag-pos-light-space)
          argset
        (let ((color :vec3 (rgb (texture diffuse-texture coord)))
              (normal :vec3 (normalize n))
              (light-color :vec3 (vec3 0.3))
              (light-dir :vec3 (normalize (- light-pos frag-pos))))
          (setf out-color
                  (vec4
                   (* color
                      (+ (* 0.3 color) ; ambient.
                         (- 1.0 (calculate-shadow frag-pos-light-space)))
                      (+ (* light-color (max 0.0 (dot light-dir normal))) ; diffuse.
                         (* light-color ; specular.
                            (pow ; spec.
                             (max 0.0
                                  (dot normal
                                   (normalize ; halway dir.
                                    (+ light-dir
                                       (normalize (- view-pos frag-pos))))))
                             64.0))))
                   1.0)))))))

(fude-gl:defvertices plane-base-vao *framebuffer-plane-vertices*
  :shader 'shadow-mapping
  :attributes '(fude-gl:xyz fude-gl:rgb fude-gl:st))

(defun framebuffer-shadow-base ()
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :w 800
                           :h 600
                           :title "Framebuffer shadow base"))
    (sdl2:with-gl-context (context win))
    (fude-gl:with-shader () (gl:enable :depth-test))
    (let* ((camera (fude-gl:make-camera))
           (light-position (3d-vectors:vec3 -2 4 -1))
           (near-plane 1.0) ; When projection is perspective,
           (far-plane 7.5) ; these vars will be refered.
           (light-space-matrix
            (3d-matrices:m*
              (3d-matrices:mortho -10 10 -10 10 near-plane far-plane)
              (3d-matrices:mlookat light-position (3d-vectors:vec3 0 0 0)
                                   (3d-vectors:vec3 0 1 0))))))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit :depth-buffer-bit)
                             :color '(0.1 0.1 0.1 1))
      ;; 1. render depth of scene to texture (from light's perspective)
      ;; render scene from light's point of view
      (fude-gl:with-framebuffer (depth-map (:depth-buffer-bit)
                                           :color nil :win win)
        (setf (fude-gl:uniform 'simple-depth "lightSpaceMatrix")
                light-space-matrix)
        (render-scene 'plane-vao))
      (gl:clear :color-buffer-bit :depth-buffer-bit)
      ;; 2. render scene as normal using the generated depth/shadow map
      (fude-gl:with-uniforms (projection view view-pos light-pos
                              (lsm "lightSpaceMatrix")
                              (diffuse-texture :unit 0) (shadow-map :unit 1))
          'shadow-mapping
        (setf lsm light-space-matrix
              view (fude-gl:view camera)
              projection
                (3d-matrices:mperspective 60
                                          (multiple-value-call #'/
                                            (sdl2:get-window-size win))
                                          0.1 100)
              view-pos (fude-gl:camera-position camera)
              light-pos light-position
              diffuse-texture
                (fude-gl:find-texture 'wood :if-does-not-exist :create)
              shadow-map
                (fude-gl:framebuffer-texture
                  (fude-gl:find-framebuffer 'depth-map)))
        (render-scene 'plane-base-vao))
      ;; render Depth map to quad for visual debugging
      (fude-gl:with-uniforms (#++ (np "nearPlane")
                              #++ (fp "farPlane") (depth-map :unit 0))
          'debug-quad
        (setf #| Comment outted, only need with perspective projection.
              np near-plane
              fp far-plane ;|#
                depth-map
              (fude-gl:framebuffer-texture
                (fude-gl:find-framebuffer 'depth-map)))
        #++
        (fude-gl:draw 'shadow-quad)))))