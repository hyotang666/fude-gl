(in-package :cl-user)

(defpackage :fude-gl
  (:use :cl)
  (:export ;;;; DEFSHADER
           #:defshader
           ;;;; FUNDAMENTAL-CLASSES
           #:xy
           #:xyz
           #:st
           #:rgb
           #:offset
           #:a
           ;;;; DEFVERTICES
           #:defvertices
           #:in-vertices
           #:with-shader
           ;;;; HELPERS
           #:uniform
           #:find-shader
           #:instances-buffer
           ;;;; GENERIC-FUNCTIONS
           #:draw
           #:send
           ;;;; TEXT-RENDERING
           #:deftexture
           #:with-textures
           #:in-texture
           #:with-text-renderer
           #:render-text
           #:glyph ; shader-class
           #:with-glyph
           #:*font-size*
           #:font-loader
           ;;;; CAMERA
           ;; constructor
           #:make-camera
           ;; readers
           #:camera-position
           #:camera-front
           #:camera-up
           ;; helpers
           #:view
           #:move
           ;;;; GL-OBJECTS
           ;; texture
           #:connect
           ;; vertex-array
           #:vertex-array
           #:in-vertex-array
           ;; BUFFER
           #:buffer
           #:in-buffer
           #:buffer-target
           #:buffer-source
           ;;;; UTILITIES
           #:list-all-textures
           #:list-all-vertices
           #:radians
           #:ortho
           #:model-matrix
           #:with-clear
           #:tex-image-2d))

(in-package :fude-gl)

;;;; VERTEX-ATTRIBUTE-CLASSES

(defparameter *vertex-attributes* (make-hash-table))

;; METACLASS

(defclass vector-class (standard-class) ())

(defmethod c2mop:validate-superclass ((c vector-class) (s standard-class)) t)

;; METACLASS for attributes.

(defclass attributes (vector-class) ())

;; METACLASS for instanced-array.

(defclass instanced-array (vector-class) ())

;; CONSTRUCTOR

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

(defmethod make-instance :around ((c vector-class) &rest args)
  (let ((values
         (loop :for initarg :in (class-initargs c)
               :collect (or (getf args initarg)
                            (error "~S is required." initarg)))))
    (make-array (length values)
                :initial-contents values
                :element-type 'single-float)))

;;;; DEFINE-VERTEX-ATTRIBUTE

(defmacro define-vertex-attribute (name (&rest slot*) &body option*)
  `(progn
    (defclass ,name ()
      ,(mapcar
         (lambda (slot)
           `(,(intern (format nil "%~A" slot) :fude-gl) :initarg
             ,(intern (string slot) :keyword) :type single-float))
         (or slot* (coerce (symbol-name name) 'list)))
      (:metaclass
       ,(if (second (assoc :instances option*))
            'instanced-array
            'attributes)))
    (setf (gethash ',name *vertex-attributes*) ',name)))

(defun pprint-define-vertex-attribute (stream exp)
  (funcall
    (formatter
     #.(apply #'concatenate 'string
              (alexandria:flatten
                (list "~:<" ; ppirnt logical block
                      "~W~^ ~1I~@_" ; operator.
                      "~W~^ ~@_" ; name
                      (list "~:<" ; pprint-logical-block of slots
                            "~@{~W~^ ~@_~}" ; slots.
                            "~:>~^ ~_")
                      "~@{~W~^ ~_~}" ; options.
                      "~:>"))))
    stream exp))

(set-pprint-dispatch '(cons (member define-vertex-attribute))
                     'pprint-define-vertex-attribute)

;; CLASSES

(define-vertex-attribute xy ())

(define-vertex-attribute xyz ())

(define-vertex-attribute st ())

(define-vertex-attribute rgb ())

(define-vertex-attribute offset (x y) (:instances t))

(define-vertex-attribute frag (bool) (:instances t))

(define-vertex-attribute a () (:instances t))

;;;; GENERIC-FUNCTIONS

(defgeneric vertex-shader (name)
  (:documentation "Return vertex shader code string."))

(defgeneric fragment-shader (name)
  (:documentation "Return fragment shader code string."))

(defgeneric uniforms (name)
  (:documentation "Return associated uniform symbols."))

;;;; DEFSHADER

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
          string)))

(defun uniform-keywordp (thing) (and (symbolp thing) (string= '&uniform thing)))

(defun <shader-forms> (shader-clause* superclasses name version)
  (let ((format
         (formatter
          #.(concatenate 'string "#version ~A core~%" ; version
                         "~{~@[~A~]in ~A ~A;~%~}~&" ; in
                         "~{out ~A ~A;~%~}~&" ; out
                         "~@[~{uniform ~A ~A;~%~}~]~&" ; uniforms
                         "void main () {~%~{~A~^~%~}~%}" ; the body.
                         ))))
    (labels ((defs (list)
               (loop :for (name type . vector-size) :in list
                     :collect nil
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
                   (type shader-lambda-list &rest main)
                   shader
                 (let* ((&uniform
                         (position-if #'uniform-keywordp shader-lambda-list))
                        (vars
                         (and shader-lambda-list
                              (defs (subseq shader-lambda-list 0 &uniform)))))
                   (rec rest vars
                        (cons
                          (<shader-method>
                            (intern (format nil "~A-SHADER" type) :fude-gl)
                            name main
                            (format nil format version in (remove nil vars)
                                    (and &uniform
                                         (delete nil
                                                 (defs
                                                   (subseq shader-lambda-list
                                                           (1+ &uniform)))))
                                    main))
                          acc))))))
      (rec shader-clause*
           (loop :for c :in (mapcar #'find-class superclasses)
                 :for slots = (c2mop:class-direct-slots c)
                 :for i :upfrom 0
                 :when slots
                   :collect (format nil "layout (location = ~A) " i)
                   :and :collect (format nil "~[~;float~:;~:*vec~D~]"
                                         (length slots))
                   :and :collect (change-case:camel-case
                                   (symbol-name (class-name c))))
           nil))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; NOTE: CHECK-BNF in DEFSHADER needs this eval-when.
  (defun vertex-attribute-p (thing)
    (and (symbolp thing) (values (gethash thing *vertex-attributes*)))))

(defmacro defshader (&whole whole name version superclasses &body shader*)
  (check-bnf:check-bnf (:whole whole)
    ((name symbol))
    ((version unsigned-byte))
    (((superclass+ superclasses) (satisfies vertex-attribute-p)))
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

;;;; GENERIC-FUNCTIONS

(defgeneric construct (thing)
  (:documentation "Requesting openGL to construct objects."))

(defgeneric destruct (thing)
  ;; NOTE!
  ;; When code fails while constructing,
  ;; DESTRUCT may be called before bound slot.
  (:documentation "Requesting openGL to destruct objects."))

(defvar *vertices* (make-hash-table :test #'eq))

(defun list-all-vertices ()
  (loop :for name :being :each :hash-key :of *vertices*
        :collect name))

(defun find-vertices (name &key (construct t) (error t))
  (let ((vertices))
    (cond ((typep name 'vertices) name)
          ((null (setf vertices (gethash name *vertices*)))
           (when error
             (error
               "Missing vertices named ~S. Eval (fude-gl:list-all-vertices)"
               name)))
          ((slot-boundp vertices 'program) vertices)
          ((not construct) vertices)
          (t
           (restart-case (construct vertices)
             (continue ()
                 :report "Return vertices without constructing."
               vertices))))))

(define-compiler-macro draw (&whole whole thing)
  (when (constantp thing)
    (find-vertices (eval thing) :construct nil :error t))
  whole)

(defgeneric draw (thing) (:method ((name symbol)) (draw (find-vertices name))))

(defgeneric send (object to &key))

;;; BUFFER

(deftype buffer-usage () '(member :static-draw :stream-draw :dynamic-draw))

(deftype buffer-target ()
  '(member :array-buffer :element-array-buffer
           :copy-read-buffer :copy-write-buffer
           :pixel-unpack-buffer :pixel-pack-buffer
           :query-buffer :texture-buffer
           :transform-feedback-buffer :uniform-buffer
           :draw-indirect-buffer :atomic-counter-buffer
           :dispatch-indirect-buffer :shader-storage-buffer))

(defstruct buffer
  name
  original
  source
  buffer
  (target :array-buffer :type buffer-target :read-only t)
  (usage :static-draw :type buffer-usage :read-only t))

(defmethod print-object ((o buffer) stream)
  (if *print-escape*
      (print-unreadable-object (o stream :type t)
        (format stream "~S ~:[unconstructed~;~:*~A~] ~S ~S" (buffer-name o)
                (buffer-buffer o) (buffer-target o) (buffer-usage o)))
      (call-next-method)))

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

(defvar *buffer*)

(defun in-buffer (buffer)
  (setf *buffer* buffer)
  (gl:bind-buffer (buffer-target buffer) (buffer-buffer buffer)))

(defmethod send ((o gl:gl-array) (to buffer) &key (method #'gl:buffer-data))
  (with-slots (target usage)
      to
    (in-buffer to)
    (cond ((eq #'gl:buffer-data method) (funcall method target usage o))
          ((eq #'gl:buffer-sub-data method) (funcall method target o))
          (t (error "Unknown method ~S" method)))))

(defmethod send ((o (eql :buffer)) (to symbol) &key (method #'gl:buffer-data))
  (let ((buffer (buffer (find-vertices to))))
    (send (buffer-source buffer) buffer :method method)))

(defmethod construct ((o buffer))
  (with-slots (original source buffer)
      o
    (unless source
      (setf source (make-gl-vector original)
            buffer (gl:gen-buffer))
      (send source o)))
  o)

(defmethod destruct ((o buffer))
  (with-slots (source buffer)
      o
    (and source (gl:free-gl-array source))
    (and buffer (gl:delete-buffers (list buffer)))
    (setf source nil
          buffer nil)))

;;;; VERTICES

(defclass vertices ()
  ((shader :type symbol :reader shader)
   (program :type unsigned-byte :reader program)
   (buffer :type buffer :reader buffer)
   (vertex-array :type unsigned-byte :reader vertex-array)))

(defmethod initialize-instance :after
           ((o vertices) &key name buffer array shader)
  (setf (slot-value o 'shader) (or shader name)
        (slot-value o 'buffer)
          (apply #'make-buffer :name :vertices :original array buffer)))

(defmethod send
           ((o 3d-matrices:mat4) (to symbol)
            &key (uniform (alexandria:required-argument :uniform)))
  (in-vertices to)
  (gl:uniform-matrix (uniform uniform to) 4 (vector (3d-matrices:marr o))))

(defmethod send
           ((o 3d-vectors:vec3) (to symbol)
            &key (uniform (alexandria:required-argument :uniform)))
  (3d-vectors:with-vec3 (x y z)
      o
    (gl:uniformf (uniform uniform to) x y z)))

(defmethod send
           ((o vector) (to symbol)
            &key (uniform (alexandria:required-argument :uniform)))
  (gl:uniformfv (uniform uniform to) o))

;; Trivial readers.

(defmethod vertex-array ((o symbol)) (vertex-array (find-vertices o)))

;; CONSTRUCTOR

(defun find-attribute (attribute class)
  (loop :for c :in (c2mop:class-direct-superclasses (find-class class))
        :for index :upfrom 0
        :when (eq attribute (class-name c))
          :return (values c index)
        :finally (error "Missing attribute ~S in ~S" attribute
                        (c2mop:class-direct-superclasses (find-class class)))))

(defun count-attributes (class)
  (loop :for c :in (c2mop:class-direct-superclasses (find-class class))
        :when (typep c 'attributes)
          :sum (length (c2mop:class-slots (c2mop:ensure-finalized c)))))

(defun attribute-offset (attribute class)
  (loop :for c :in (c2mop:class-direct-superclasses (find-class class))
        :until (eq attribute (class-name c))
        :sum (length (c2mop:class-slots c))))

(defun link-attribute (attribute class)
  (multiple-value-bind (c index)
      (find-attribute attribute class)
    (let* ((slots (c2mop:class-slots (c2mop:ensure-finalized c)))
           (type (foreign-type (c2mop:slot-definition-type (car slots)))))
      (gl:enable-vertex-attrib-array index)
      (etypecase c
        (attributes
         (gl:vertex-attrib-pointer index (length slots) type nil
                                   (* (count-attributes class)
                                      (cffi:foreign-type-size type))
                                   (* (attribute-offset attribute class)
                                      (cffi:foreign-type-size type))))
        (instanced-array
         (gl:vertex-attrib-pointer index (length slots) type nil
                                   (* (length slots)
                                      (cffi:foreign-type-size type))
                                   0)
         (%gl:vertex-attrib-divisor index 1))))))

(defun link-attributes (class instance-buffers)
  (loop :for c :in (c2mop:class-direct-superclasses (find-class class))
        :do (in-buffer (construct (funcall instance-buffers (class-name c))))
            (link-attribute (class-name c) class)))

(defvar *vertex-array*)

(defun in-vertex-array (vao) (gl:bind-vertex-array (setf *vertex-array* vao)))

(defun compile-shader (prog vertex-shader fragment-shader)
  (let ((vs (gl:create-shader :vertex-shader))
        (fs (gl:create-shader :fragment-shader)))
    (unwind-protect
        (labels ((compile-s (prog id source)
                   (gl:shader-source id source)
                   (gl:compile-shader id)
                   (may-warn (gl:get-shader-info-log id))
                   (gl:attach-shader prog id))
                 (may-warn (log)
                   (unless (equal "" log)
                     (warn log))))
          (compile-s prog vs vertex-shader)
          (compile-s prog fs fragment-shader)
          (gl:link-program prog)
          (may-warn (gl:get-program-info-log prog)))
      (gl:delete-shader fs)
      (gl:delete-shader vs))))

(defun create-program (name)
  (let ((program (gl:create-program)))
    (when (zerop program)
      (error "Fails to create program."))
    (compile-shader program (vertex-shader name) (fragment-shader name))
    program))

(defmethod construct ((o vertices))
  (with-slots (program vertex-array shader)
      o
    (setf program (create-program shader)
          vertex-array (create-vertex-array o))
    o))

(defmethod destruct ((o vertices))
  (when (slot-boundp o 'program)
    (gl:delete-program (program o))
    (slot-makunbound o 'program))
  (when (slot-boundp o 'vertex-array)
    (gl:delete-vertex-arrays (list (vertex-array o)))
    (slot-makunbound o 'vertex-array))
  (destruct (buffer o)))

(defmethod draw :before ((o vertices)) (in-vertices o))

(defmethod draw ((o vertices)) (gl:draw-arrays :triangles 0 (vertex-length o)))

;;;; INDEXED

(defclass indexed-vertices (vertices) ((indices :type buffer :reader indices)))

(defmethod initialize-instance :after
           ((o indexed-vertices) &key indices &allow-other-keys)
  (setf (slot-value o 'indices)
          (apply #'make-buffer :name :indices :original
                 (coerce (car indices) '(array (unsigned-byte 8) (*)))
                 (append (cdr indices) (list :target :element-array-buffer)))))

(defmethod construct ((o indexed-vertices))
  (with-slots (program vertex-array shader)
      o
    (setf program (create-program shader)
          vertex-array (create-vertex-array o))
    o))

(defmethod destruct ((o indexed-vertices))
  (call-next-method)
  (destruct (indices o)))

(defmethod draw ((o indexed-vertices))
  (draw-elements :triangles (buffer-original (indices o))))

;; CONSTRUCTOR
;;;; INSTANCED-SHADER

(defclass instanced () ((table :initform nil :accessor table)))

(defmethod initialize-instance :after
           ((o instanced) &key instances &allow-other-keys)
  (loop :for (name original . options) :in instances
        :do (push
             (cons name
                   (apply #'make-buffer :name :instanced :original original
                          options))
             (table o))))

(defclass instanced-vertices (vertices instanced) ())

(defmethod construct ((o instanced-vertices))
  (loop :for (nil . buffer) :in (table o)
        :do (construct buffer))
  (with-slots (program vertex-array shader)
      o
    (setf program (create-program shader)
          vertex-array (create-vertex-array o))
    o))

(defmethod destruct ((o instanced-vertices))
  (loop :for (nil . buffer) :in (table o)
        :do (destruct buffer)
            (call-next-method)))

(defun vertex-length (vertices)
  (/ (length (buffer-original (buffer vertices)))
     (count-attributes (shader vertices))))

(defmethod draw ((o instanced-vertices))
  (%gl:draw-arrays-instanced :triangles 0 (vertex-length o)
                             (array-dimension
                               (buffer-original (cdar (table o))) 0)))

(defun instances-buffer (vertices name)
  (or (cdr (assoc name (table (find-vertices vertices))))
      (error "Missing vertices ~S in ~S" name vertices)))

(defmethod send ((o symbol) (to symbol) &key (method #'gl:buffer-data))
  (let ((buffer (instances-buffer to o)))
    (send (buffer-source buffer) buffer :method method)))

(defgeneric create-vertex-array (vertices)
  (:method :around ((o vertices))
    (let ((vao (gl:gen-vertex-array)))
      (in-vertex-array vao)
      (call-next-method)
      vao))
  (:method ((o vertices)) (link-attributes (shader o) (constantly (buffer o))))
  (:method ((o indexed-vertices)) (construct (indices o)) (call-next-method))
  (:method ((o instanced-vertices))
    (loop :for (nil . buffer) :in (table o)
          :do (construct buffer))
    (link-attributes (shader o)
                     (let ((table (table o)) (default-buffer (buffer o)))
                       (lambda (slot)
                         (or (cdr (assoc slot table)) default-buffer))))))

;;;; HELPERS

(define-compiler-macro uniform (&whole whole name vertices)
  (when (constantp vertices)
    (let ((vertices (eval vertices)))
      (find-vertices vertices :construct nil :error t)
      (when (constantp name)
        (let ((name (eval name)))
          (assert (find (subseq name 0 (position #\[ name)) (uniforms vertices)
                        :test #'string=
                        :key (lambda (s) (change-case:camel-case (string s))))
            ()
            "Unknown uniform ~S for ~S" name (uniforms vertices))))))
  whole)

(defun uniform (name vertices)
  (gl:get-uniform-location (program (find-vertices vertices)) name))

;;;; DEFVERTICES

(defmacro defvertices (name array &rest options)
  (unless (getf options :shader)
    (assert (find-class name)))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name *vertices*)
             ,(cond
                ((getf options :indices)
                 `(make-instance 'indexed-vertices :name ',name :array ,array
                                 ,@options))
                ((getf options :instances)
                 `(make-instance 'instanced-vertices :name ',name :array ,array
                                 ,@options))
                (t
                 `(make-instance 'vertices :name ',name :array ,array
                                 ,@options))))
     ',name))

(defun pprint-defvertices (stream exp)
  (funcall
    (formatter
     #.(apply #'concatenate 'string
              (alexandria:flatten
                (list "~:<" ; pprint-logical-block.
                      "~W~^ ~1I~@_" ; operator.
                      "~W~^ ~3I~:_" ; name
                      "~W~^ ~1I~_" ; array
                      "~@{~W~^ ~@_~W~^ ~_~}" ; k-v pair options.
                      "~:>"))))
    stream exp))

(set-pprint-dispatch '(cons (member defvertices)) 'pprint-defvertices)

;;;; WITH-SHADER

(defvar *toplevel-p* t)

(defmacro with-shader (() &body body)
  (let ((toplevelp (gensym "TOPLEVELP")))
    `(let ((,toplevelp *toplevel-p*) (*toplevel-p* nil))
       (unwind-protect (progn ,@body)
         (when ,toplevelp
           (loop :for shader :being :each :hash-value :of *vertices*
                 :when (slot-boundp shader 'program)
                   :do (destruct shader)))))))

(defun pprint-with-shader (stream exp)
  (funcall
    (formatter
     #.(apply #'concatenate 'string
              (alexandria:flatten
                (list "~:<" ; pprint-logical-block
                      "~W~^ ~1I~@_" ; operator
                      (list "~:<" ; dummy
                            "~@{~W~^ ~:_~}" ; dummy
                            "~:>~^ ~_")
                      "~@{~W~^ ~_~}" ; the body.
                      "~:>"))))
    stream exp))

(set-pprint-dispatch '(cons (member with-shader)) 'pprint-with-shader)

(defmacro in-vertices (form)
  (when (constantp form)
    (find-vertices (eval form) :construct nil :error t))
  `(let ((shader (find-vertices ,form)))
     (gl:use-program (program shader))
     shader))

(defun connect (shader &rest pairs)
  (in-vertices shader)
  (loop :for i :upfrom 0
        :for (uniform name) :on pairs :by #'cddr
        :for texture = (find-texture name)
        :do (gl:uniformi (uniform uniform shader) i)
            (gl:active-texture i)
            (gl:bind-texture (texture-target texture) (texture-id texture))))

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
    (ecase direction
      (:top-down (3d-matrices:mortho 0 w h 0 -1 1))
      (:bottom-up (3d-matrices:mortho 0 w 0 h -1 1)))))

(defun model-matrix (x y w h &optional (rotate 0))
  (3d-matrices:nmscale
    (3d-matrices:nmtranslate
      (3d-matrices:nmrotate
        (3d-matrices:nmtranslate
          (3d-matrices:mtranslation (3d-vectors:vec x y 0))
          (3d-vectors:vec (* 0.5 w) (* 0.5 h) 0))
        3d-vectors:+vz+ (radians rotate))
      (3d-vectors:vec (* -0.5 w) (* -0.5 h) 0))
    (3d-vectors:vec w h 1)))

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
  '(or texture-mag-filter
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

(defvar *textures* (make-hash-table :test #'eq))

(defstruct texture
  (id nil :type (or null unsigned-byte))
  (params nil :type list :read-only t)
  (target (alexandria:required-argument :target)
          :type texture-target
          :read-only t)
  (initializer (alexandria:required-argument :initializer)
               :type function
               :read-only t))

(defmethod construct ((o texture))
  (with-slots (id target params initializer)
      o
    (unless id
      (setf id (car (gl:gen-textures 1)))
      (gl:bind-texture target id)
      (loop :for (k v) :on params :by #'cddr
            :do (gl:tex-parameter target k v))
      (funcall initializer)))
  o)

(defmethod destruct ((o texture))
  (when (texture-id o)
    (gl:delete-textures (list (texture-id o)))
    (setf (texture-id o) nil)))

(defmacro deftexture (name target init-form &body options)
  (type-assert target 'texture-target)
  `(progn
    (setf (gethash ',name *textures*)
            (make-texture :target ,target
                          :initializer (lambda () ,init-form)
                          :params ',(loop :for (k v) :on options :by #'cddr
                                          :with spec
                                                = (list :texture-wrap-s :repeat
                                                        :texture-wrap-t :repeat
                                                        :texture-min-filter :linear
                                                        :texture-mag-filter :linear)
                                          :do (type-assert k 'texture-pname)
                                              (setf (getf spec k) v)
                                          :finally (return spec))))
    ',name))

(defun pprint-deftexture (stream exp)
  (funcall
    (formatter
     #.(apply #'concatenate 'string
              (alexandria:flatten
                (list "~:<" ; pprint-logical-block
                      "~W~^ ~1I~@_" ; operator
                      "~W~^ ~@_" ; name
                      "~W~^ ~_" ; target
                      "~W~^ ~_" ; init-form
                      "~@{~W~^ ~@_~W~^ ~_~}" ; k-v pair options.
                      "~:>"))))
    stream exp))

(set-pprint-dispatch '(cons (member deftexture)) 'pprint-deftexture)

(defun list-all-textures ()
  (loop :for name :being :each :hash-key :of *textures*
        :collect name))

(defun find-texture (name &key (construct t) (error t))
  (let ((texture (or (gethash name *textures*))))
    (cond
      ((null texture)
       (when error
         (error "Missing texture named ~S. Eval (fude-gl:list-all-textures)"
                name)))
      ((texture-id texture) texture)
      ((not construct) texture)
      (t
       (restart-case (construct texture)
         (continue ()
             :report "Return texture without constructing."
           texture))))))

(defmacro in-texture (name)
  `(let ((texture (find-texture ,name)))
     (gl:bind-texture (texture-target texture) (texture-id texture))))

(defmacro with-textures (() &body body)
  `(unwind-protect (progn ,@body)
     (loop :for texture :being :each :hash-value :of *textures*
           :when (texture-id texture)
             :do (destruct texture))))

(defun pprint-with-textures (stream exp)
  (funcall
    (formatter
     #.(apply #'concatenate 'string
              (alexandria:flatten
                (list "~:<" ; pprint logial block.
                      "~1I~W~^ ~@_" ; operator
                      (list "~:<" ; dummy
                            "~@{~W~^ ~:_~}" ; dummy body
                            "~:>~^ ~_")
                      "~@{~W~^ ~_~}" ; The body.
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

;;;; CAMERA

(defstruct (camera (:constructor make-camera
                    (&key (position (3d-vectors:vec3 0 0 3))
                     (target (3d-vectors:vec3 0 0 0))
                     (front (3d-vectors:vec3 0 0 -1)) &aux
                     (direction
                      (3d-vectors:nvunit (3d-vectors:v- position target)))
                     (right
                      (3d-vectors:nvunit
                        (3d-vectors:vc (3d-vectors:vec3 0 1 0) direction)))
                     (up (3d-vectors:vc direction right)))))
  (position (alexandria:required-argument :position)
            :type 3d-vectors:vec3
            :read-only t)
  (target (alexandria:required-argument :target)
          :type 3d-vectors:vec3
          :read-only t)
  (front (alexandria:required-argument :front)
         :type 3d-vectors:vec3
         :read-only t)
  ;; NOTE: This slot may not needed.
  (right (alexandria:required-argument :right)
         :type 3d-vectors:vec3
         :read-only t)
  (up (alexandria:required-argument :up) :type 3d-vectors:vec3 :read-only t))

(defun view (camera &key target)
  (3d-matrices:mlookat (camera-position camera)
                       (if target
                           (camera-target camera)
                           (3d-vectors:v+ (camera-position camera)
                                          (camera-front camera)))
                       (camera-up camera)))

(defun move (camera x y z)
  (3d-vectors:vsetf (camera-position camera) x y z)
  camera)
