(in-package :cl-user)

(defpackage :fude-gl
  (:use :cl)
  (:export ;;;; MAIN API.
           #:defshader
           #:with-shader
           ;;;; FUNDAMENTAL-CLASSES
           #:xy
           #:xyz
           #:st
           #:rgb
           ;;;; TEXT-RENDERING
           #:with-text-renderer
           #:render-text
           #:glyph ; shader-class
           #:with-glyph
           #:*font-size*
           #:font-loader
           ;;;; GL-OBJECTS
           ;; texture
           #:texture
           #:texture-id
           ;; program
           #:in-shader
           ;; vertex-array
           #:in-vertex-array
           ;;;; UTILITIES
           #:radians
           #:with-gl-vector
           #:with-textures
           #:with-2d-textures
           #:with-clear
           #:foreign-type
           #:indices-of
           #:draw-elements
           #:tex-image-2d))

(in-package :fude-gl)

;;;; GL-OBJECT

(defstruct gl-object
  (name (alexandria:required-argument :name)
        :type (or character symbol)
        :read-only t)
  (id (alexandria:required-argument :id) :type (unsigned-byte 32) :read-only t))

;;;; UTILITIES
;; MACROS

(defun type-assert (form type)
  (if (constantp form)
      (progn
       (assert (typep form type) ()
         "~S is not type of ~S" form (millet:type-expand type))
       form)
      `(the ,type ,form)))

;; MATRIX

(defun radians (degrees) (* degrees (/ pi 180)))

(defun ortho (win &optional (direction :bottom-up))
  (multiple-value-bind (w h)
      (sdl2:get-window-size win)
    (3d-matrices:marr
      (ecase direction
        (:top-down (3d-matrices:mortho 0 w h 0 -1 1))
        (:bottom-up (3d-matrices:mortho 0 w 0 h -1 1))))))

;;;; CLASSES

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun class-list (class)
    "Return class list specified to abstract oder, superclasses are reverse order."
    (uiop:while-collecting (acc)
      (labels ((rec (c)
                 (unless (eq 'standard-object (class-name c))
                   (acc c)
                   (mapc #'rec (reverse (c2mop:class-direct-superclasses c))))))
        (rec class))))
  (defun class-initargs (class)
    (uiop:while-collecting (acc)
      (dolist (c (nreverse (class-list class)))
        (dolist (s (c2mop:class-direct-slots c))
          (acc (car (c2mop:slot-definition-initargs s)))))))
  ;; METACLASS
  (defclass vector-class (standard-class) ())
  (defmethod c2mop:validate-superclass ((c vector-class) (s standard-class)) t)
  ;; CLASSES
  #| NOTE:
 | T is constant.
 | We could not name a slot with constant (at least in sbcl).
 | So we decide to use prefix % for every slot name.
 | We choice having single rule rather than having corner case.
 |#
  (defclass xy ()
    ((%x :initarg :x :type single-float) (%y :initarg :y :type single-float))
    (:metaclass vector-class))
  (defclass xyz ()
    ((%x :initarg :x :type single-float)
     (%y :initarg :y :type single-float)
     (%z :initarg :z :type single-float))
    (:metaclass vector-class))
  (defclass st ()
    ((%s :initarg :s :type single-float) (%t :initarg :t :type single-float))
    (:metaclass vector-class))
  (defclass rgb ()
    ((%r :initarg :r :type single-float)
     (%g :initarg :g :type single-float)
     (%b :initarg :b :type single-float))
    (:metaclass vector-class)))

;;;; CONSTRUCTOR

(defmethod make-instance :around ((c vector-class) &rest args)
  (let ((values
         (loop :for initarg :in (class-initargs c)
               :collect (or (getf args initarg)
                            (error "~S is required." initarg)))))
    (make-array (length values)
                :initial-contents values
                :element-type 'single-float)))

;;;; GENERIC-FUNCTIONS

(defgeneric vertex-shader (name)
  (:documentation "Return vertex shader code string."))

(defgeneric fragment-shader (name)
  (:documentation "Return fragment shader code string."))

(defgeneric uniforms (name)
  (:documentation "Return associated uniform symbols."))

;;;; DSL
;;; DEFSHADER

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun <uniforms> (name shader*)
    `(defmethod uniforms ((type (eql ',name)))
       (list
         ,@(loop :for (nil lambda-list) :in shader*
                 :for position
                      = (position-if
                          (lambda (x) (and (symbolp x) (string= '&uniform x)))
                          lambda-list)
                 :when position
                   :nconc (let ((acc))
                            (dolist (x (subseq lambda-list (1+ position)) acc)
                              (pushnew `',(car x) acc :test #'equal)))))))
  (defun <shader-method> (method name main string)
    `(defmethod ,method ((type (eql ',name)))
       ,(if (typep main '(cons (cons (eql quote) (cons symbol null)) null))
            `(,method ',(cadar main))
            string))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun uniform-keywordp (thing)
    (and (symbolp thing) (string= '&uniform thing)))
  (defun <shader-forms> (shader-clause* superclasses name version)
    (let ((format
           (formatter
            #.(concatenate 'string "#version ~A core~%" ; version
                           "~{in ~A ~A;~%~}~&" ; in
                           "~{out ~A ~A;~%~}~&" ; out
                           "~@[~{uniform ~A ~A;~%~}~]~&" ; uniforms
                           "void main () {~%~{~A~^~%~}~%}" ; the body.
                           ))))
      (labels ((defs (list)
                 (loop :for (name type . vector-size) :in list
                       :collect (change-case:camel-case (symbol-name type))
                       :collect (if vector-size
                                    (format nil "~A[~A]"
                                            (change-case:camel-case
                                              (symbol-name name))
                                            (car vector-size))
                                    (change-case:camel-case
                                      (symbol-name name)))))
               (rec (shaders in acc)
                 (if (endp shaders)
                     (nreverse acc)
                     (body (car shaders) (cdr shaders) in acc)))
               (body (shader rest in acc)
                 (destructuring-bind
                     (type out &rest main)
                     shader
                   (let* ((&uniform (position-if #'uniform-keywordp out))
                          (vars (and out (defs (subseq out 0 &uniform)))))
                     (rec rest vars
                          (cons
                            (<shader-method>
                              (intern (format nil "~A-SHADER" type) :fude-gl)
                              name main
                              (format nil format version in vars
                                      (and &uniform
                                           (defs (subseq out (1+ &uniform))))
                                      main))
                            acc))))))
        (rec shader-clause*
             (loop :for c
                        :in (mapcan (lambda (c) (class-list (find-class c)))
                                    superclasses)
                   :for slots = (c2mop:class-direct-slots c)
                   :when slots
                     :collect (format nil "vec~D" (length slots))
                     :and :collect (change-case:camel-case
                                     (symbol-name (class-name c))))
             nil)))))

(defmacro defshader (&whole whole name version superclasses &body shader*)
  (check-bnf:check-bnf (:whole whole)
    ((name symbol))
    ((version unsigned-byte))
    (((superclass+ superclasses) symbol))
    ((shader* (or vertex-clause fragment-clause))
     ;;
     (vertex-clause ((eql :vertex) shader-lambda-list main*))
     ;;
     (fragment-clause ((eql :fragment) shader-lambda-list main*))
     ;;
     (shader-lambda-list (out-spec* uniform-keyword? uniform-spec*))
     (out-spec (var type-key))
     (uniform-keyword (satisfies uniform-keywordp))
     (uniform-spec (var type-key vector-size?))
     (vector-size unsigned-byte)
     ;;
     (var symbol)
     (type-key keyword)
     (main check-bnf:expression)))
  ;; The body.
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defclass ,name ,superclasses () (:metaclass vector-class))
     ,@(<shader-forms> shader* superclasses name version)
     (with-prog ((check (vertex-shader ',name) (fragment-shader ',name))))
     ,(<uniforms> name shader*)
     ',name))

(defun pprint-defshader (stream exp)
  (setf stream (or stream *standard-output*))
  (funcall
    (formatter
     #.(apply #'concatenate 'string
              (alexandria:flatten
                (list "~:<" ; pprint-logical-block.
                      "~W~^ ~1I~@_" ; operator.
                      "~W~^ ~@_" ; name.
                      "~W~^ ~@_" ; version.
                      (list "~:<" ; superclasses
                            "~@{~W~^ ~:_~}" ; each class.
                            "~:>~^ ~:_")
                      (list "~@{" ; shaders
                            (list "~:<" ; each clause.
                                  "~W~^ ~1I~@_" ; key
                                  (list "~:<" ; out lambda list.
                                        "~@{~W~^ ~:_~}" ; out lambda var.
                                        "~:>~^ ~_")
                                  "~@{~W~^ ~_~}" ; clause body.
                                  "~:>~^ ~_")
                            "~}")
                      "~:>"))))
    stream exp))

(set-pprint-dispatch '(cons (member defshader)) 'pprint-defshader)

;;;; UTILITIES
;;; WITH-GL-ARRAY

(defun foreign-type (cl-type &key cffi)
  (cond ((and cffi (subtypep cl-type 'single-float)) :float)
        ((and cffi (subtypep cl-type 'double-float)) :double)
        ((subtypep cl-type 'float) :float)
        ((subtypep cl-type '(unsigned-byte 8))
         (if cffi
             :unsigned-char
             :unsigned-byte))
        ((subtypep cl-type '(signed-byte 8))
         (if cffi
             :char
             :byte))
        ((subtypep cl-type '(unsigned-byte 16)) :unsigned-short)
        ((subtypep cl-type '(signed-byte 16)) :short)
        ((subtypep cl-type '(unsigned-byte 32)) :unsigned-int)
        ((subtypep cl-type '(signed-byte 32)) :int)
        ((and cffi (subtypep cl-type '(unsigned-byte 64))) :unsigned-long)
        ((and cffi (subtypep cl-type '(signed-byte 64))) :long)
        (t (error "Not supported type. ~S" cl-type))))

(defun make-gl-vector (initial-contents)
  (let* ((length (array-total-size initial-contents))
         (a
          (gl:alloc-gl-array
            (foreign-type (array-element-type initial-contents) :cffi t)
            length)))
    (dotimes (i length a)
      (setf (gl:glaref a i) (row-major-aref initial-contents i)))))

(defmacro with-gl-vector (&whole whole (&rest bind*) &body body)
  "Each var is bound by gl-array."
  (check-bnf:check-bnf (:whole whole) ((bind* (symbol check-bnf:expression))))
  `(let ,(loop :for (var array) :in bind*
               :collect `(,var (make-gl-vector ,array)))
     (unwind-protect (progn ,@body)
       ,@(mapcar (lambda (bind) `(gl:free-gl-array ,(car bind))) bind*))))

;;; WITH-BUFFER
;; GL enum types.

(deftype buffer-usage () '(member :static-draw :stream-draw :dynamic-draw))

(deftype buffer-target ()
  '(member :array-buffer :element-array-buffer
           :copy-read-buffer :copy-write-buffer
           :pixel-unpack-buffer :pixel-pack-buffer
           :query-buffer :texture-buffer
           :transform-feedback-buffer :uniform-buffer
           :draw-indirect-buffer :atomic-counter-buffer
           :dispatch-indirect-buffer :shader-storage-buffer))

;; BUFFER object.

(defstruct (buffer (:include gl-object))
  (target :array-buffer :type buffer-target :read-only t)
  (usage :static-draw :type buffer-usage :read-only t))

(defvar *buffers* nil "Dynamic buffer environment.")

(defvar *buffer* :uninitizlied-buffer "Current buffer.")

(declaim (type list *buffers*)
         (type (or (eql :uninitialized-buffer) buffer) *buffer*))

(defun find-buffer (thing)
  (etypecase thing
    (buffer thing)
    (symbol
     (or (find thing *buffers* :key #'buffer-name)
         (error "Missing buffer named ~S. ~S" thing *buffers*)))))

(defmacro in-buffer (form)
  (let ((buffer (gensym "BUFFER")))
    `(let ((,buffer (find-buffer ,form)))
       (gl:bind-buffer (buffer-target ,buffer) (buffer-id ,buffer))
       (setf *buffer* ,buffer))))

(defmacro with-buffer (&whole whole (&rest bind*) &body body)
  (check-bnf:check-bnf (:whole whole)
    ((bind* (var buffer-option*))
     (buffer-option* option-key keyword)
     (option-key (member :target :usage))
     (var symbol)))
  `(destructuring-bind
       ,(mapcar #'car bind*)
       (mapcar
         (lambda (bind id)
           (destructuring-bind
               (name &key (target :array-buffer) (usage :static-draw))
               bind
             (make-buffer :id id :name name :target target :usage usage)))
         ',bind* (gl:gen-buffers ,(length bind*)))
     (unwind-protect
         (let ((*buffer* *buffer*)
               (*buffers* (list* ,@(mapcar #'car bind*) *buffers*)))
           ,@body)
       (gl:delete-buffers
         (list ,@(mapcar (lambda (bind) `(buffer-id ,(car bind))) bind*))))))

;;; WITH-PROG

(defvar *progs* nil)

(defvar *prog* :uninitialized-program)

(defstruct (program (:include gl-object)))

(defun find-program (thing)
  (etypecase thing
    (program thing)
    (symbol
     (or (find thing *progs* :key #'program-name)
         (error "Missing program named ~S: ~S" thing *progs*)))))

(defmacro in-shader (form)
  (let ((program (gensym "PROGRAM")))
    `(let ((,program (find-program ,form)))
       (gl:use-program (program-id ,program))
       (setf *prog* ,program))))

(defun compile-shader (prog vertex-shader fragment-shader)
  (let ((vs (gl:create-shader :vertex-shader))
        (fs (gl:create-shader :fragment-shader)))
    (unwind-protect
        (labels ((compile-s (prog id source)
                   (gl:shader-source id source)
                   (gl:compile-shader id)
                   (may-warn (gl:get-shader-info-log id))
                   (gl:attach-shader (program-id prog) id))
                 (may-warn (log)
                   (unless (equal "" log)
                     (warn log))))
          (compile-s prog vs vertex-shader)
          (compile-s prog fs fragment-shader)
          (gl:link-program (program-id prog))
          (may-warn (gl:get-program-info-log (program-id prog))))
      (gl:delete-shader fs)
      (gl:delete-shader vs))))

(defmacro with-prog (&whole whole (&rest bind*) &body body)
  (check-bnf:check-bnf (:whole whole)
    ((bind* (symbol check-bnf:expression check-bnf:expression))))
  `(let* ((*prog* *prog*)
          ,@(loop :for (name) :in bind*
                  :collect `(,name
                             (make-program :name ',name
                                           :id (gl:create-program))))
          (*progs* (list* ,@(mapcar #'car bind*) *progs*)))
     (unwind-protect
         (progn
          ,@(loop :for bind :in bind*
                  :collect `(compile-shader ,@bind))
          ,@body)
       ,@(mapcar (lambda (bind) `(gl:delete-program (program-id ,(car bind))))
                 bind*))))

;;; LINK-ATTRIBUTES

(defun get-attrib-location (program class)
  (let* ((name (change-case:camel-case (symbol-name (class-name class))))
         (loc (gl:get-attrib-location (program-id program) name)))
    (if (minusp loc)
        (error "Not active attribute name ~S in ~S." name program)
        loc)))

(defun link-attributes (class program)
  (labels ((rec (class-list total-length funs)
             (if (endp class-list)
                 (let ((total (apply #'+ total-length)))
                   (loop :for f :in funs
                         :for l :in total-length
                         :do (funcall f total l offset)
                         :sum l :into offset))
                 (let ((slots
                        (length (c2mop:class-direct-slots (car class-list)))))
                   (if (zerop slots)
                       (rec (cdr class-list) total-length funs)
                       (rec (cdr class-list)
                            (cons (the (integer 1 4) slots) total-length)
                            (cons (processer (car class-list)) funs))))))
           (processer (class)
             (lambda (total-length length offset)
               (let* ((location (get-attrib-location program class))
                      (slots
                       (c2mop:class-direct-slots
                         (c2mop:ensure-finalized class)))
                      (type
                       (ecase (c2mop:slot-definition-type (car slots))
                         (single-float :float)))
                      (size (cffi:foreign-type-size type)))
                 #++
                 (uiop:format! *trace-output* "~%Length ~S. Offset ~S."
                               (* total-length size) (* offset size))
                 (gl:vertex-attrib-pointer location length type nil ; As
                                                                    ; normalized-p
                                           (* total-length size)
                                           (* offset size))
                 (gl:enable-vertex-attrib-array location)))))
    (rec (class-list (find-class class)) nil nil)))

;;; WITH-VERTEX-ARRAY

(defstruct (vertex-array (:include gl-object)))

(defvar *vertex-arrays* nil)

(defvar *vertex-array* :uninitialzied-vertex-array)

(defun find-vertex-array (thing)
  (etypecase thing
    (vertex-array thing)
    (symbol
     (or (find thing *vertex-arrays* :key #'vertex-array-name)
         (error "Missing vertex-array named ~S. ~S" thing *vertex-arrays*)))))

(defmacro in-vertex-array (form)
  (let ((vao (gensym "VERTEX-ARRAY")))
    `(let ((,vao (find-vertex-array ,form)))
       (gl:bind-vertex-array (vertex-array-id ,vao))
       (setf *vertex-array* ,vao))))

(defmacro with-vertex-array (&whole whole (&rest bind*) &body body)
  (check-bnf:check-bnf (:whole whole)
    ((bind* (symbol init-form+))
     (init-form+ check-bnf:expression)))
  `(let* ((*vertex-array* *vertex-array*)
          ,@(loop :for (name) :in bind*
                  :collect `(,name
                             (make-vertex-array :name ',name
                                                :id (gl:gen-vertex-array))))
          (*vertex-arrays* (list* ,@(mapcar #'car bind*) *vertex-arrays*)))
     (unwind-protect
         (progn
          ,@(mapcan
              (lambda (bind) `((in-vertex-array ',(car bind)) ,@(cdr bind)))
              bind*)
          ,@body)
       (gl:delete-vertex-arrays
         (list
           ,@(mapcar (lambda (bind) `(vertex-array-id ,(car bind))) bind*))))))

;;; WITH-TEXTURES

(deftype texture-wrapping ()
  '(member :repeat :mirrored-repeat :clamp-to-edge :clamp-to-border))

(deftype texture-target ()
  '(member :texture-1d :texture-1d-array
           :texture-2d :texture-2d-array
           :texture-2d-multisample :texture-2d-multisample-array
           :texture-3d :texture-cube-map
           :texture-cube-map-array :texture-rectangle))

(deftype texture-pname ()
  '(member :depth-stencil-texture-mode
           :texture-base-level :texture-compare-func
           :texture-compare-mode :texture-lod-bias
           :texture-min-filter :texture-mag-filter
           :texture-min-lod :texture-max-lod
           :texture-max-level :texture-swizzle-r
           :texture-swizzle-g :texture-swizzle-b
           :texture-swizzle-a :texture-wrap-s
           :texture-wrap-t :texture-wrap-r))

(deftype texture-mag-filter () '(member :linear :nearest))

(deftype texture-min-filter ()
  '(or textute-mag-filter
       (member :nearest-mipmap-nearest :lenear-mipmap-nearest
               :nearest-mipmap-linear :linear-mipmap-linear)))

(deftype base-internal-format ()
  '(member :depth-component :depth-stencil :red :rg :rgb :rgba))

(deftype pixel-format ()
  '(or base-internal-format
       (member :bgr
               :bgra :red-integer
               :rg-integer :rgb-integer
               :bgr-integer :rgba-integer
               :bgra-integer :stencil-index)))

(defstruct (texture (:include gl-object))
  (target (alexandria:required-argument :target)
          :type texture-target
          :read-only t))

(defvar *textures* nil)

(defvar *texture* :uninitialized-texture)

(defun find-texture (thing)
  (etypecase thing
    (texture thing)
    ((or character symbol)
     (or (find thing *textures* :key #'texture-name)
         (error "Missing texture named ~S. ~S" thing *textures*)))))

(defmacro in-texture (form)
  (let ((texture (gensym "TEXTURE")))
    `(let ((,texture (find-texture ,form)))
       (gl:active-texture (texture-id ,texture))
       (gl:bind-texture (texture-target ,texture) (texture-id ,texture))
       (setf *texture* ,texture))))

(defmacro with-textures ((&rest bind*) &body body)
  "Each VAR is bound by openGL texture id."
  ;; Trivial syntax check.
  (dolist (b bind*) (the (cons symbol (cons texture-target *)) b))
  (labels ((vname (k v)
             (case k
               ((:texture-wrap-s :texture-wrap-t :texture-wrap-r)
                (type-assert v 'texture-wrapping))
               ((:texture-mag-filter) (type-assert v 'texture-mag-filter))
               ((:texture-min-fileter) (type-assert v 'texture-min-filter))
               (otherwise v)))
           (<option-setters> (params target)
             (destructuring-bind
                 (&key (texture-wrap-s :repeat) (texture-wrap-t :repeat)
                  (texture-min-filter :linear) (texture-mag-filter :linear)
                  &allow-other-keys)
                 params
               (let ((params
                      (list* :texture-wrap-s texture-wrap-s :texture-wrap-t
                             texture-wrap-t :texture-mag-filter
                             texture-mag-filter :texture-min-filter
                             texture-min-filter
                             (uiop:remove-plist-keys
                               '(:texture-wrap-s :texture-wrap-t
                                 :texture-min-filter :texture-mag-filter)
                               params))))
                 (loop :for (k v) :on params :by #'cddr
                       :collect `(gl:tex-parameter ,target
                                                   ,(type-assert k
                                                                 'texture-pname)
                                                   ,(vname k v)))))))
    ;; The body.
    `(destructuring-bind
         ,(mapcar #'car bind*)
         (loop :for (name target) :in ',bind*
               :for id :in (gl:gen-textures ,(length bind*))
               :collect (make-texture :id id :name name :target target))
       (unwind-protect
           (let ((*texture* *texture*)
                 (*textures* (list* ,@(mapcar #'car bind*) *textures*)))
             ,@(mapcan
                 (lambda (b)
                   (destructuring-bind
                       (var target &key params init)
                       b
                     `((in-texture ',var) ,@(<option-setters> params target)
                       ,@(when init
                           `(,init)))))
                 bind*)
             ,@body)
         (gl:delete-textures
           (list
             ,@(mapcar (lambda (bind) `(texture-id ,(car bind))) bind*)))))))

(defun pprint-with-textures (stream exp)
  (funcall
    (formatter
     #.(apply #'concatenate 'string
              (alexandria:flatten
                (list "~:<" ; pprint-logical-block
                      "~W~^ ~1I~@_" ; operator.
                      (list "~:<" ; binds
                            "~@{" ; iterate binds.
                            (list "~:<" ; each bind
                                  "~W~^ ~:I~@_" ; var
                                  "~W~^ ~_" ; target.
                                  "~@{~W~^ ~@_~W~^ ~_~}" ; k-v options.
                                  "~:>~^ ~_")
                            "~}" ; end of iterate.
                            "~:>~^ ~:@_")
                      "~@{~W~^ ~_~}" ; the body.
                      "~:>"))))
    stream exp))

(set-pprint-dispatch '(cons (member with-textures)) 'pprint-with-textures)

(defun tex-image-2d (array)
  (let ((format (ecase (array-dimension array 2) (3 :rgb) (4 :rgba))))
    (gl:tex-image-2d (the texture-target :texture-2d) 0 ; mipmap level.
                     (the base-internal-format format)
                     (array-dimension array 0) ; width
                     (array-dimension array 1) ; height
                     0 ; legacy stuff.
                     (the pixel-format format)
                     (foreign-type (array-element-type array))
                     (make-array (array-total-size array)
                                 :element-type (array-element-type array)
                                 :displaced-to array))))

;;;; WITH-VAO

(defmacro indices-of (id)
  (declare (ignore id))
  (error "INDICE-OF is must be inside of WITH-VAO."))

(defun get-uniform-location (program name)
  (let ((location (gl:get-uniform-location (program-id program) name)))
    (assert (not (minusp location)) ()
      "Uniform ~S is not active in program ~S." name program)
    location))

(defun <uniform-binder> (prog)
  (lambda (uniform)
    (etypecase uniform
      (symbol
       `(,uniform
         (get-uniform-location ,prog
                               ,(change-case:camel-case
                                  (symbol-name uniform)))))
      ((cons symbol (cons symbol null))
       `(,(first uniform)
         (get-uniform-location ,prog
                               ,(change-case:camel-case
                                  (symbol-name (second uniform)))))))))

(defun ensure-second (thing)
  (if (listp thing)
      (second thing)
      thing))

(defun <init-buffer> (buf vec)
  `((in-buffer ',buf)
    (gl:buffer-data (buffer-target ,buf) (buffer-usage ,buf) ,vec)))

(defun prog-name (prog bind*)
  (or (and (symbol-package prog) prog) (caar bind*)))

(defun uniform-bind (bind* prog)
  (let* ((uniforms (cdr (assoc :uniform (cdar bind*))))
         (required (uniforms (prog-name prog bind*)))
         (actual (mapcar #'ensure-second uniforms)))
    (assert (null (set-exclusive-or required actual :test #'string=)) ()
      "Mismatch uniforms. ~S but ~S" required actual)
    (mapcar (<uniform-binder> prog) uniforms)))

(defun parse-with-vao-binds (bind* body)
  (let ((refs))
    (labels ((clause (clause bind)
               (or (assoc clause (cdr bind))
                   (error "Missing required cluase ~S in ~S" clause bind)))
             (rec (bind*)
               (if (endp bind*)
                   body
                   (destructuring-bind
                       (prog vs fs)
                       (cdr (clause :shader (car bind*)))
                     (unless prog
                       (setf prog (gensym "PROG")))
                     `((with-prog ((,prog ,vs ,fs))
                         ,(body (assoc :indices (cdar bind*)) prog bind*))))))
             (<may-uniform-bind> (uniforms bind*)
               (if uniforms
                   `((let ,uniforms
                       (declare (ignorable ,@(mapcar #'car uniforms)))
                       ,@(rec (cdr bind*))))
                   (rec (cdr bind*))))
             (<body-form> (bind* prog &optional indices-bind ebo-bind ebo-inits)
               (let* ((verts (clause :vertices (car bind*)))
                      (vertices (or (second verts) (gensym "VERTICES")))
                      (vbo
                       `(,(or (cadr (assoc :buffer (cdar bind*)))
                              (gensym "VBO"))
                         ,@(cdddr (assoc :vertices (cdar bind*)))))
                      (uniforms (uniform-bind bind* prog))
                      (attr (second (clause :attributes (car bind*)))))
                 `(with-gl-vector ((,vertices ,(third verts)) ,@indices-bind)
                    (with-buffer ,(list* vbo ebo-bind)
                      (with-vertex-array ((,(caar bind*)
                                           ,@(<init-buffer> (car vbo) vertices)
                                           (in-shader ',prog)
                                           (link-attributes ,attr ,prog)
                                           ,@ebo-inits))
                        ,@(<may-uniform-bind> uniforms bind*))))))
             (body (vec prog bind*)
               (if vec
                   (alexandria:with-unique-names (vector indices ebo)
                     `(let ((,vector ,(second vec)))
                        ,(progn
                          (push (list (prog-name prog bind*) `',vector) refs)
                          (<body-form> bind* prog `((,indices ,vector))
                                       `((,ebo
                                          ,@(uiop:remove-plist-key :size (cddr
                                                                           vec))))
                                       (<init-buffer> ebo indices)))))
                   (<body-form> bind* prog))))
      (values (rec bind*) refs))))

(defmacro with-vao (&whole whole (&rest bind*) &body body)
  "Each VAR is bound by openGL vertex array object id."
  (check-bnf:check-bnf (:whole whole)
    ((bind* (var option+))
     (option+
      (or vertices-clause
          indices-clause
          uniform-clause
          buffer-clause
          attributes-clause
          shader-clause))
     ;; When this VAR is specified, it is bound by gl-array pointer.
     (vertices-clause ((eql :vertices) var init-form vertices-option*))
     (vertices-option* (member :usage :target :size) check-bnf:expression)
     ;;
     (indices-clause ((eql :indices) init-form indices-option*))
     (indices-option* keyword check-bnf:expression)
     ;; When this VAR is specified, it is bound by openGL uniform location.
     (uniform-clause ((eql :uniform) uniform-var-spec+))
     (uniform-var-spec (or var (var var)))
     ;; When this VAR is specified, it is bound by buffer object.
     (buffer-clause ((eql :buffer) var))
     ;;
     (attributes-clause ((eql :attributes) attribute-name))
     (attribute-name check-bnf:expression)
     ;; When this VAR is specified, it is bound by program object.
     (shader-clause ((eql :shader) var vertex-shader fragment-shader))
     (vertex-shader check-bnf:expression)
     (fragment-shader check-bnf:expression)
     ;;
     (var symbol)
     (init-form check-bnf:expression)))
  (multiple-value-bind (forms refs)
      (parse-with-vao-binds bind* body)
    (if (null refs)
        (car forms)
        `(macrolet ((indices-of (id)
                      (case id ,@refs (otherwise "No indices for ~S" id))))
           ,@forms))))

;;; WITH-SHADER

(defmacro with-shader (&whole whole (&rest bind*) &body body)
  (check-bnf:check-bnf (:whole whole)
    ((bind* (symbol option+))
     (option+ (option-name option-form+))
     (option-name
      (member :vertices
              :indices :uniform
              :buffer :attributes
              :shader :vertex-array))
     (option-form+ check-bnf:expression)))
  `(with-vao ,(mapcar
                (lambda (bind)
                  (destructuring-bind
                      (class &rest clause*)
                      bind
                    `(,(or (cadr (assoc :vertex-array (cdr bind)))
                           (gensym "VAO"))
                      ,@(loop :for clause :in clause*
                              :when (eq :indices (car clause))
                                :collect `(:indices
                                           (coerce ,(second clause)
                                                   '(array (unsigned-byte 8)
                                                     (*)))
                                           :target :element-array-buffer)
                              :else :unless (eq :vertex-array (car clause))
                                :collect clause)
                      (:attributes ',(alexandria:ensure-car class))
                      (:shader ,class
                       (vertex-shader ',(alexandria:ensure-car class))
                       (fragment-shader ',(alexandria:ensure-car class))))))
                bind*)
     ,@body))

(defun pprint-with-shader (stream exp)
  (funcall
    (formatter
     #.(apply #'concatenate 'string
              (alexandria:flatten
                (list "~:<" ; Pprint-logical-block.
                      "~W~^ ~1I" ; Operator.
                      (list "~:<" ; Binds
                            "~@{" ; clauses.
                            (list "~:<" ; Each bind clause.
                                  "~@{" ; Clause.
                                  "~W~^ ~1I~_" ; Var
                                  "~@{" ; Each options
                                  (list "~:<" ; option.
                                        "~W~^ ~:I~@_" ; option key.
                                        "~@{~W~^ ~:_~}" ; option body.
                                        "~:>~^ ~_")
                                  "~}" ; Options.
                                  "~}" ; End clause.
                                  "~:>~^ ~_")
                            "~}" ; clauses end.
                            "~:>~^ ~_")
                      "~@{~W~^ ~_~}" ; Body.
                      "~:>"))))
    stream exp))

(set-pprint-dispatch '(cons (member with-shader)) 'pprint-with-shader)

;;;; WITH-CLEAR

(deftype buffer-bit ()
  '(member :color-buffer-bit :depth-buffer-bit :stencil-buffer-bit))

(defmacro with-clear
          (&whole whole
           (var-win (&rest buf*) &key (color ''(0.0 0.0 0.0 1.0)) (fps 60))
           &body body)
  (check-bnf:check-bnf (:whole whole)
    ((var-win symbol))
    ((buf* check-bnf:expression))
    ((color check-bnf:expression)))
  (let ((time (gensym "TIME")) (idle (gensym "IDLE")) (delta (gensym "DELTA")))
    `(let ((,time (get-internal-real-time))
           (,idle ,(* internal-time-units-per-second (/ 1 fps))))
       (apply #'gl:clear-color ,color)
       (gl:clear ,@(mapcar (lambda (buf) (type-assert buf 'buffer-bit)) buf*))
       ,@body
       (sdl2:gl-swap-window ,var-win)
       (let ((,delta (- ,idle (- (get-internal-real-time) ,time))))
         (if (plusp ,delta)
             (sleep (* ,(/ 1 internal-time-units-per-second) ,delta))
             (warn "Over FPS. ~S sec."
                   (float
                     (* (/ 1 internal-time-units-per-second) (- ,delta)))))))))

(defun pprint-with-clear (stream exp)
  (funcall
    (formatter
     #.(apply #'concatenate 'string
              (alexandria:flatten
                (list "~:<" ; pprint-logical-block
                      "~W~^ ~1I~@_" ; operator.
                      (list "~:<" ; options
                            "~^~W~^ ~:I~@_" ; var-win.
                            (list "~:<" ; bufs
                                  "~@{~W~^ ~@_~}" ; each buf.
                                  "~:>~^ ~_")
                            "~@{~W~^ ~@_~W~^ ~:_~}" ; k-v pairs
                            "~:>~^ ~_")
                      "~@{~W~^ ~_~}" ; body
                      "~:>"))))
    stream exp))

(set-pprint-dispatch '(cons (member with-clear)) 'pprint-with-clear)

;;;; DRAW-ELEMENTS

(deftype draw-mode ()
  '(member :points :line-strip
           :line-loop :lines
           :line-strip-adjacency :lines-adjacency
           :triangle-strip :triangle-fan
           :triangles :tiangle-strip-adjacency
           :triangles-adjacency :patches))

(defun draw-elements (mode cl-vector &key (offset 0))
  (%gl:draw-elements mode (length cl-vector)
                     (foreign-type (array-element-type cl-vector)) offset))

;;;; ENABLE-CAPABILITIES

(deftype enable-capabilities ()
  '(member :blend :clip-distance
           :color-logic-op :cull-face
           :debug-output :debug-output-synchronous
           :depth-clamp :depth-test
           :dither :framebuffer-srgb
           :line-smooth :multisample
           :polygon-offset-fill :polygon-offset-line
           :polygon-offset-point :polygon-smooth
           :primitive-restart :primitive-restart-fixed-index
           :rastarizer-discard :sample-alpha-to-coverage
           :sample-alpha-to-one :sample-coverage
           :sample-shading :sample-mask
           :scissor-test :stencil-test
           :texture-cube-map-seamless :program-point-size))

(deftype source-factor ()
  '(or dest-factor
       (member :src-alpha-saturate
               :src1-color :one-minus-src1-color
               :src1-alpha :one-minus-src1-alpha)))

(deftype dest-factor ()
  '(member :zero :one
           :src-color :one-minus-src-color
           :dst-color :one-minus-dst-color
           :src-alpha :one-minus-src-alpha
           :dst-alpha :one-minus-dst-alpha
           :constant-color :one-minus-constant-color
           :constant-alpha :one-minus-constant-alpha))

;;;; FONT

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
  (texture (alexandria:required-argument :texture) :type texture :read-only t)
  w
  h
  bearing-x
  bearing-y
  advance)

(defshader glyph 330 (xy st)
  (:vertex ((|texCoords| :vec2) &uniform (projection :mat4))
    "texCoords = st;"
    "gl_Position = projection * vec4(xy, 0.0, 1.0);")
  (:fragment ((color :vec4) &uniform (text :|sampler2D|) (|textColor| :vec3))
    "color = vec4(textColor, 1.0) * vec4(1.0, 1.0, 1.0, texture(text, texCoords).r);"))

(defvar *glyphs*)

(defmacro with-glyph ((&key (size '*font-size*)) &body body)
  `(let ((*fonts* (alexandria:copy-hash-table *fonts*))
         (*glyphs* (make-hash-table))
         (*font-size* ,size))
     (unwind-protect (progn ,@body)
       (loop :for g :being :each :hash-value of *glyphs*
             :collect (texture-id (char-glyph-texture g)) :into textures
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
              (let ((texture
                     (make-texture :id (car (gl:gen-textures 1))
                                   :name char
                                   :target :texture-2d)))
                (in-texture texture)
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
       (text shader
        &key (x 0) (y 0) (scale 1) (color '(1 1 1)) (font "Ubuntu-M")
        (vertices (error ":VERTICES is required."))
        (color-uniform (error ":COLOR-UNIFORM is required."))
        ((:vertex-array vao) (error ":VERTEX-ARRAY is required."))
        ((:vertex-buffer vbo) (error ":VERTEX-BUFFER is required."))
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
  (in-shader shader)
  (apply #'gl:uniformf color-uniform color)
  (gl:active-texture 0)
  (in-vertex-array vao)
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
                  :do (setf (gl:glaref vertices i) (float elt)))
            (gl:bind-texture (texture-target (char-glyph-texture glyph))
                             (texture-id (char-glyph-texture glyph)))
            (in-buffer vbo)
            (gl:buffer-sub-data (buffer-target vbo) vertices)
            (gl:draw-arrays :triangles 0 6)
            (incf x (* scale (char-glyph-advance glyph)))))

(defmacro with-text-renderer
          ((name &key (size 16) (win (alexandria:required-argument :win)))
           &body body)
  (alexandria:with-unique-names (vertices vao buffer projection text text-color)
    `(with-shader ((glyph
                     (:vertices ,vertices
                                (make-array (* 4 6)
                                            :element-type 'single-float
                                            :initial-element 0.0)
                                :usage :dynamic-draw)
                     (:vertex-array ,vao)
                     (:buffer ,buffer)
                     (:uniform (,projection projection) (,text text)
                               (,text-color |textColor|))))
       (in-shader glyph)
       (gl:uniform-matrix ,projection 4 (vector (ortho ,win)))
       (with-glyph (:size ,size)
         (flet ((,name (string &key (x 0) (y 0) (scale 1))
                  (render-text string glyph
                               :color-uniform ,text-color
                               :vertices ,vertices
                               :vertex-array ,vao
                               :vertex-buffer ,buffer
                               :scale scale
                               :x x
                               :y y
                               :win ,win)))
           ,@body)))))
