(in-package :fude-gl)

(defun initialize-fonts (root)
  (let ((ht (make-hash-table :test #'equal)))
    (uiop:collect-sub*directories root #'identity ; always true.
                                  #'identity ; recurse all directories.
                                  (lambda (dir)
                                    (loop :for pathname
                                               :in (uiop:directory-files dir
                                                                         "*.ttf")
                                          :do (setf (gethash
                                                      (pathname-name pathname)
                                                      ht)
                                                      pathname))))
    ht))

(defparameter *fonts* (initialize-fonts "/usr/share/fonts/"))

(defparameter *font-size* 16)

(defun find-font (name &optional (errorp t))
  (or (values (gethash name *fonts*))
      (and errorp (error "Missing font named: ~S" name))))

(defun list-all-fonts ()
  (loop :for k :being :each :hash-key :of *fonts*
        :collect k))

(defun font-loader (font-name)
  (let ((loader (find-font font-name nil)))
    (typecase loader
      (zpb-ttf::font-loader loader)
      ((or string pathname)
       (setf (gethash font-name *fonts*) (zpb-ttf::open-font-loader loader)))
      (otherwise
       (error
         "Unknown font. ~S ~:_Eval (fude-gl:list-all-fonts) for supported fonts."
         font-name)))))

(defstruct char-glyph
  (texture (alexandria:required-argument :texture)
           :type unsigned-byte
           :read-only t)
  w
  h
  bearing-x
  bearing-y
  advance)

(defshader glyph 330 (xy st)
  (:vertex ((|texCoords| :vec2) &uniform (projection :mat4))
    (declaim (ftype (function nil (values)) main))
    (defun main ()
      "texCoords = st;"
      "gl_Position = projection * vec4(xy, 0.0, 1.0);"))
  (:fragment ((color :vec4) &uniform (text :|sampler2D|) (|textColor| :vec3))
    (declaim (ftype (function nil (values)) main))
    (defun main ()
      "color = vec4(textColor, 1.0) * vec4(1.0, 1.0, 1.0, texture(text, texCoords).r);")))

(defvertices glyph
    (make-array (* 4 6) :element-type 'single-float :initial-element 0.0)
  :buffer '(:usage :dynamic-draw))

(defvar *glyphs*)

(defmacro with-glyph ((&key (size '*font-size*)) &body body)
  `(let ((*fonts* (alexandria:copy-hash-table *fonts*))
         (*glyphs* (make-hash-table))
         (*font-size* ,size))
     (unwind-protect (progn ,@body)
       (loop :for g :being :each :hash-value of *glyphs*
             :collect (char-glyph-texture g) :into textures
             :finally (gl:delete-textures textures))
       (loop :for v :being :each :hash-value of *fonts*
             :when (typep v 'zpb-ttf::font-loader)
               :do (zpb-ttf::close-font-loader v)))))

(defun pprint-with-glyph (stream exp)
  (funcall
    (formatter
     #.(apply #'concatenate 'string
              (alexandria:flatten
                (list "~:<" ; Pprint-logical-block.
                      "~W~^ ~1I~@_" ; Operator.
                      (list "~:<" ; Pprint-logical-block for option.
                            "~@{~W~^ ~@_~}" ; each elt.
                            "~:>~^ ~_")
                      "~@{~W~^ ~_~}" ; The body.
                      "~:>"))))
    stream exp))

(set-pprint-dispatch '(cons (member with-glyph)) 'pprint-with-glyph)

(defun font-data (char loader size)
  (flet ((non-zero-int (i)
           (if (zerop i)
               1
               i)))
    (let* ((string (string char))
           (bbox (vecto:string-bounding-box string size loader))
           (w
            (ceiling
              (non-zero-int (- (zpb-ttf:xmax bbox) (zpb-ttf:xmin bbox)))))
           (h
            (ceiling
              (non-zero-int (- (zpb-ttf:ymax bbox) (zpb-ttf:ymin bbox))))))
      ;; TODO Implement gray scale rasterizer.
      (vecto:with-canvas (:width w :height h)
        (vecto:set-font loader size)
        (vecto:draw-string (- (zpb-ttf:xmin bbox)) (- (zpb-ttf:ymin bbox))
                           string)
        (values (loop :with vec = (vecto::image-data vecto::*graphics-state*)
                      :with new
                            = (make-array (* w h)
                                          :element-type '(unsigned-byte 8)
                                          :initial-element 0)
                      :for i :upfrom 3 :by 4
                      :while (array-in-bounds-p vec i)
                      :do (setf (aref new (floor i 4)) (aref vec i))
                      :finally (return new))
                w
                h
                (floor (zpb-ttf:xmin bbox))
                (ceiling (zpb-ttf:ymax bbox))
                (ceiling
                  (* (zpb-ttf:advance-width (zpb-ttf:find-glyph char loader))
                     (/ size (zpb-ttf:units/em loader)))))))))

(defun char-glyph (char font-name &optional (size *font-size*))
  (let ((loader (font-loader font-name)))
    (if (not (zpb-ttf:glyph-exists-p char loader))
        (error "~S is not exist in the font ~S." char font-name)
        (or (gethash char *glyphs*)
            (multiple-value-bind (image w h bearing-x bearing-y advance)
                (font-data char loader size)
              (gl:pixel-store :unpack-alignment 1)
              (let ((texture (car (gl:gen-textures 1))))
                (gl:active-texture 0)
                (gl:bind-texture :texture-2d texture)
                (gl:tex-image-2d :texture-2d 0 :red w h 0 :red
                                 :unsigned-byte image)
                (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
                (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
                (gl:tex-parameter :texture-2d :texture-min-filter :linear)
                (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
                (setf (gethash char *glyphs*)
                        (make-char-glyph :texture texture
                                         :w w
                                         :h h
                                         :bearing-x bearing-x
                                         :bearing-y bearing-y
                                         :advance advance))))))))

(defun render-text
       (text
        &key (x 0) (y 0) (scale 1) (color '(1 1 1)) (font "Ubuntu-M")
        (win
         (when (or (eq :center x) (eq :center y))
           (alexandria:required-argument :win))))
  (when win
    (let ((bbox
           (vecto:string-bounding-box text (ceiling (* scale *font-size*))
                                      (font-loader font))))
      (when (eq :center x)
        (setf x (- (floor (sdl2:get-window-size win) 2)
                   (floor (- (zpb-ttf:xmax bbox) (zpb-ttf:xmin bbox)) 2))))
      (when (eq :center y)
        (setf y (- (floor (nth-value 1 (sdl2:get-window-size win)) 2)
                   (floor (- (zpb-ttf:ymax bbox) (zpb-ttf:ymin bbox)) 2))))))
  (setf text (map 'list (lambda (c) (char-glyph c font)) text))
  (with-slots (vertex-array buffer)
      (in-vertices 'glyph)
    (let ((source (buffer-source buffer)))
      (apply #'gl:uniformf (uniform 'glyph "textColor") color)
      (gl:active-texture 0)
      (gl:bind-vertex-array vertex-array)
      (loop :for glyph :in text
            :for x-pos = (+ x (* (char-glyph-bearing-x glyph) scale))
            :for y-pos
                 = (- y
                      (* (- (char-glyph-h glyph) (char-glyph-bearing-y glyph))
                         scale))
            :for w = (* scale (char-glyph-w glyph))
            :for h = (* scale (char-glyph-h glyph))
            :do (loop :for elt
                           :in (list x-pos (+ h y-pos) 0 0 ; upper left
                                     x-pos y-pos 0 1 ; bottom left
                                     (+ w x-pos) y-pos 1 1 ; bottom right
                                     x-pos (+ h y-pos) 0 0 ; upper left
                                     (+ w x-pos) y-pos 1 1 ; bottom right
                                     (+ w x-pos) (+ h y-pos) 1 0) ; upper right
                      :for i :upfrom 0
                      :do (setf (gl:glaref source i) (float elt)))
                (gl:bind-texture :texture-2d (char-glyph-texture glyph))
                (send source buffer :method #'gl:buffer-sub-data)
                (gl:draw-arrays :triangles 0 6)
                (incf x (* scale (char-glyph-advance glyph)))))))

(defmacro with-text-renderer
          ((name &key (size 16) (win (alexandria:required-argument :win)))
           &body body)
  `(with-shader ()
     (setf (uniform 'glyph "projection") (ortho ,win))
     (with-glyph (:size ,size)
       (flet ((,name (string &key (x 0) (y 0) (scale 1))
                (render-text string :scale scale :x x :y y :win ,win)))
         ,@body))))