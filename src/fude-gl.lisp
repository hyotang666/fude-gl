(in-package :fude-gl)

(declaim (optimize speed))

;;;; VERBOSE OPENGL

(deftype buffer-usage () '(member :static-draw :stream-draw :dynamic-draw))

(deftype buffer-target ()
  '(member :array-buffer :element-array-buffer
           :copy-read-buffer :copy-write-buffer
           :pixel-unpack-buffer :pixel-pack-buffer
           :query-buffer :texture-buffer
           :transform-feedback-buffer :uniform-buffer
           :draw-indirect-buffer :atomic-counter-buffer
           :dispatch-indirect-buffer :shader-storage-buffer))

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

(deftype pixel-type ()
  '(member :unsigned-byte :byte
           :unsigned-short :short
           :unsigned-int :int
           :half-float :float
           :unsigned-byte-3-3-2 :unsigned-byte-2-3-3-rev
           :unsigned-short-5-6-5 :unsigned-short-5-6-5-rev
           :unsigned-short-4-4-4-4 :unsigned-short-4-4-4-4-rev
           :unsigned-short-5-5-5-1 :unsigned-short-1-5-5-5-rev
           :unsigned-int-8-8-8-8 :unsigned-int-8-8-8-8-rev
           :unsigned-int-10-10-10-2 :unsigned-int-2-10-10-10-rev))

(deftype buffer-bit ()
  '(member :color-buffer-bit :depth-buffer-bit :stencil-buffer-bit))

(deftype draw-mode ()
  '(member :points :line-strip
           :line-loop :lines
           :line-strip-adjacency :lines-adjacency
           :triangle-strip :triangle-fan
           :triangles :tiangle-strip-adjacency
           :triangles-adjacency :patches))

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

(deftype attachment ()
  '(member :color-attachment0 :depth-attachment :stencil-attachment))

(deftype framebuffer-texture-target ()
  '(member :texture-2d
           :texture-cube-map-positive-x :texture-cube-map-negative-x
           :texture-cube-map-positive-y :texture-cube-map-negative-y
           :texture-cube-map-positive-z :texture-cube-map-negative-z))

(deftype get-program-pname ()
  '(member :delete-status :link-status
           :validate-status :info-log-length
           :attached-shaders :active-atomic-counter-buffers
           :active-attributes :active-attribute-max-length
           :active-uniforms :active-uniform-blocks
           :active-uniform-block-max-name-length :active-uniform-max-length
           :compute-work-group-size :program-binary-length
           :transform-feedback-buffer-mode :transform-feedback-varyings
           :transform-feedback-varying-max-length :geometry-vertices-out
           :geometry-input-type :geometry-output-type))

(define-condition missing-vertices (fude-gl-error cell-error)
  ()
  (:report
   (lambda (this output)
     (format output "Missing vertices named ~S. ~:@_" (cell-error-name this))
     (did-you-mean output (princ-to-string (cell-error-name this))
                   (list-all-vertices))
     (format output " ~:@_To see defined vertices, eval ~S"
             '(list-all-vertices)))))

(define-condition missing-definition (style-warning)
  ((condition :initarg :condition :reader %condition))
  (:report
   (lambda (this output)
     (format output
             "~:I~W ~:@_Or you may have used vertices before defining it."
             (%condition this)))))

(defvar *condition*) ; For debug use.

(declaim
 (ftype (function
         ((eql :framebuffer) attachment framebuffer-texture-target
          (unsigned-byte 32) integer)
         (values &optional))
        framebuffer-texture-2d))

(defun framebuffer-texture-2d (target attachment textarget texture level)
  (handler-bind ((condition (lambda (c) (print (setf *condition* c)))))
    (gl:framebuffer-texture-2d target attachment textarget texture level)))

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

(defun tex-image-2d (array &key (target :texture-2d))
  #+sbcl ; Due to the array dimensions are unknown in compile time.
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((format
         (typecase (array-dimensions array)
           ((cons * (cons * null)) :red)
           ((cons * (cons * (cons (eql 3) null))) :rgb)
           ((cons * (cons * (cons (eql 4) null))) :rgba)
           (otherwise (error "NIY")))))
    (gl:tex-image-2d target 0 ; mipmap level.
                     (the base-internal-format format)
                     (array-dimension array 0) ; width
                     (array-dimension array 1) ; height
                     0 ; legacy stuff.
                     (the pixel-format format)
                     (foreign-type (array-element-type array))
                     (make-array (array-total-size array)
                                 :element-type (array-element-type array)
                                 :displaced-to array))))

;;;; GENERIC-FUNCTIONS

(define-compiler-macro draw (&whole whole thing)
  (when (constantp thing)
    (handler-case (find-vertices (eval thing) :if-does-not-exist nil)
      (missing-vertices (c)
        (warn 'missing-definition :condition c))))
  whole)

(defmethod send ((o glsl-structure-object) (to symbol) &key uniform)
  (dolist (slot (c2mop:class-slots (class-of o)))
    (send (slot-value o (c2mop:slot-definition-name slot)) to
          :uniform (format nil "~A.~A" uniform
                           (symbol-camel-case
                             (c2mop:slot-definition-name slot))))))

;;;; BUFFER

(defstruct buffer
  name
  ;; Lisp side vector as meta-buffer-object, i.e. used for cl:length,
  ;; cl:array-rank, cl:array-dimensions etc...
  (original (error "ORIGINAL is required.") :type array)
  ;; GL side vector.
  source
  ;; Buffer ID.
  buffer
  (target :array-buffer :type buffer-target :read-only t)
  (usage :static-draw :type buffer-usage :read-only t))

(defmethod print-object ((o buffer) stream)
  (if *print-escape*
      (print-unreadable-object (o stream :type t)
        (funcall (formatter "~S ~:[unconstructed~;~:*~A~] ~S ~S") stream
                 (buffer-name o) (buffer-buffer o) (buffer-target o)
                 (buffer-usage o)))
      (call-next-method)))

(defun make-gl-vector (initial-contents)
  #+sbcl ; Due to unknown array rank at compile time.
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
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

(defmethod send ((o gl:gl-array) (to buffer) &key (method #'gl:buffer-data))
  (with-slots (target usage)
      to
    (in-buffer to)
    (cond ((eq #'gl:buffer-data method) (funcall method target usage o))
          ((eq #'gl:buffer-sub-data method) (funcall method target o))
          (t (error "Unknown method ~S" method)))))

;;;; VERTICES
;; VERTICES

(defclass vertices ()
  ((shader :type symbol :accessor shader)
   (attributes :type list :reader attributes)))

(defmethod initialize-instance :after
           ((o vertices) &key name shader attributes &allow-other-keys)
  (setf (slot-value o 'shader) (or shader name)
        (slot-value o 'attributes)
          (or (mapcar #'find-class attributes)
              (c2mop:class-direct-superclasses (find-class (or shader name))))))

(defclass array-vertices (vertices)
  ((draw-mode :type draw-mode :reader draw-mode)
   (buffer :type buffer :reader buffer)
   (vertex-array :type (unsigned-byte 32) :reader vertex-array)))

(defmethod initialize-instance :after
           ((o array-vertices)
            &key buffer array (draw-mode :triangles) &allow-other-keys)
  (setf (slot-value o 'buffer)
          (apply #'make-buffer :name :vertices :original array buffer)
        (slot-value o 'draw-mode) draw-mode))

(defmethod constructed-p ((o array-vertices)) (slot-boundp o 'vertex-array))

;; *VERTICES*

(defvar *vertices* (make-hash-table :test #'eq))

(defun list-all-vertices ()
  (loop :for name :being :each :hash-key :of *vertices*
        :collect name))

(defun find-vertices (name &key (if-does-not-exist :error))
  "Return VERTICES object named NAME.
The behavior when vertices are not created by GL yet depends on IF-DOES-NOT-EXIST."
  (let ((vertices))
    (cond ((typep name 'vertices) name)
          ;; Vertices are not defined yet.
          ((null (setf vertices (gethash name *vertices*)))
           (error 'missing-vertices :name name))
          ;; Vertices are already constructed in GL.
          ((constructed-p vertices) vertices)
          ;; Vertices are not constrcuted in GL yet.
          (t
           (ecase if-does-not-exist
             (:error
              (cerror "Return vertices without constructing."
                      "Vertices named ~S are not constructed in GL yet." name)
              vertices)
             (:create
              (restart-case (construct vertices)
                (continue ()
                    :report "Return vertices without constructing."
                  vertices)))
             ((nil) vertices))))))

;; Trivial readers.

(defmethod vertex-array ((o symbol))
  (vertex-array (find-vertices o :if-does-not-exist :create)))

(defmethod shader ((o symbol))
  (shader (find-vertices o :if-does-not-exist :create)))

(defmethod (setf shader) (new (o symbol))
  (setf (shader (find-vertices o :if-does-not-exist :create)) new))

;; *VERTEX-ARRAY*

(defvar *vertex-array*)

(defun in-vertex-array (vao) (gl:bind-vertex-array (setf *vertex-array* vao)))

(defmacro in-vertices (form &key (with-vertex-array t))
  (when (constantp form)
    (find-vertices (eval form) :if-does-not-exist nil))
  `(let ((vertices (find-vertices ,form :if-does-not-exist :create)))
     (in-program (shader vertices))
     ,@(when with-vertex-array
         `((in-vertex-array (vertex-array vertices))))
     vertices))

;; CONSTRUCT helpers

(defun find-attribute (attribute class)
  (loop :for c :in (c2mop:class-direct-superclasses (find-class class))
        :for index :of-type (mod #.most-positive-fixnum) :upfrom 0
        :when (eq attribute (class-name c))
          :return (values c index)
        :finally (error "Missing attribute ~S in ~S" attribute
                        (c2mop:class-direct-superclasses (find-class class)))))

(defun count-attributes (attributes)
  (let ((sum 0))
    (declare ((mod #.most-positive-fixnum) sum))
    (dolist (c attributes sum)
      (when (typep c 'attributes)
        (incf sum
              (list-length (c2mop:class-slots (c2mop:ensure-finalized c))))))))

(defun attribute-offset (attribute attributes)
  (let ((sum 0))
    (declare ((mod #.most-positive-fixnum) sum))
    (loop :for c :in attributes
          :until (eq attribute (class-name c))
          :do (incf sum (list-length (c2mop:class-slots c))))
    sum))

(defun link-attribute (a index attributes)
  (let* ((slots (c2mop:class-slots (c2mop:ensure-finalized a)))
         (type (foreign-type (c2mop:slot-definition-type (car slots)))))
    (gl:enable-vertex-attrib-array index)
    (etypecase a
      (attributes
       (gl:vertex-attrib-pointer index (length slots) type nil
                                 (the fixnum
                                      (* (count-attributes attributes)
                                         (the fixnum
                                              (cffi:foreign-type-size type))))
                                 (the fixnum
                                      (*
                                        (attribute-offset (class-name a)
                                                          attributes)
                                        (the fixnum
                                             (cffi:foreign-type-size type))))))
      (instanced-array
       (gl:vertex-attrib-pointer index (length slots) type nil
                                 (the fixnum
                                      (* (length slots)
                                         (the fixnum
                                              (cffi:foreign-type-size type))))
                                 0)
       (%gl:vertex-attrib-divisor index 1)))))

(declaim
 (ftype (function (list function) (values null &optional)) link-attributes))

(defun link-attributes (superclasses instance-buffers)
  (loop :for c :in superclasses
        :for i :of-type (mod #.most-positive-fixnum) :upfrom 0
        :do (in-buffer (construct (funcall instance-buffers (class-name c))))
            (link-attribute c i superclasses)))

(defmethod create-vertex-array :around ((o array-vertices))
  (let ((vao (gl:gen-vertex-array)))
    (in-vertex-array vao)
    (call-next-method)
    vao))

(defmethod create-vertex-array ((o array-vertices))
  (link-attributes (attributes o) (constantly (buffer o))))

(defmethod construct ((o array-vertices))
  (with-slots (vertex-array shader)
      o
    (find-program shader :if-does-not-exist :create)
    (setf vertex-array (create-vertex-array o))
    o))

(defmethod destruct ((o array-vertices))
  (when (slot-boundp o 'vertex-array)
    (gl:delete-vertex-arrays (list (vertex-array o)))
    (slot-makunbound o 'vertex-array))
  (destruct (buffer o)))

(defmethod send ((o (eql :buffer)) (to symbol) &key (method #'gl:buffer-data))
  (let ((buffer (buffer (find-vertices to :if-does-not-exist :create))))
    (send (buffer-source buffer) buffer :method method)))

(defmethod draw ((name symbol))
  (draw (find-vertices name :if-does-not-exist :create)))

(defmethod draw :before ((o array-vertices)) (in-vertices o))

(defmethod draw ((o array-vertices))
  #+sbcl ; Due to out of our responsibility.
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (gl:draw-arrays (draw-mode o) 0 (vertex-length o)))

;; INDEXED

(defclass indexed-vertices (array-vertices)
  ((indices :type buffer :reader indices)))

(defmethod initialize-instance :after
           ((o indexed-vertices) &key indices &allow-other-keys)
  (setf (slot-value o 'indices)
          (apply #'make-buffer :name :indices :original
                 (coerce (the list (car indices))
                         '(array (unsigned-byte 16) (*)))
                 (append (cdr indices) (list :target :element-array-buffer)))))

(defmethod create-vertex-array ((o indexed-vertices))
  (construct (indices o))
  (call-next-method))

(defmethod construct ((o indexed-vertices))
  (with-slots (vertex-array shader)
      o
    (find-program shader :if-does-not-exist :create)
    (setf vertex-array (create-vertex-array o))
    o))

(defmethod destruct ((o indexed-vertices))
  (call-next-method)
  (destruct (indices o)))

(defmethod draw ((o indexed-vertices))
  (let ((cl-vector (buffer-original (indices o))))
    (%gl:draw-elements (draw-mode o) (length cl-vector)
                       (foreign-type (array-element-type cl-vector)) 0)))

;; INSTANCED

(defclass instanced () ((table :initform nil :accessor table)))

(defmethod initialize-instance :after
           ((o instanced) &key instances &allow-other-keys)
  (loop :for (name original . options) :in instances
        :do (push
             (cons name
                   (apply #'make-buffer :name :instanced :original original
                          options))
             (table o))))

(defclass instanced-vertices (array-vertices instanced) ())

(defmethod create-vertex-array ((o instanced-vertices))
  (loop :for (nil . buffer) :in (table o)
        :do (construct buffer))
  (link-attributes (attributes o)
                   (let ((table (table o)) (default-buffer (buffer o)))
                     (lambda (slot)
                       (declare (symbol slot))
                       (or (cdr (assoc slot table)) default-buffer)))))

(defmethod construct ((o instanced-vertices))
  (loop :for (nil . buffer) :in (table o)
        :do (construct buffer))
  (with-slots (vertex-array shader)
      o
    (find-program shader :if-does-not-exist :create)
    (setf vertex-array (create-vertex-array o))
    o))

(defmethod destruct ((o instanced-vertices))
  (loop :for (nil . buffer) :in (table o)
        :do (destruct buffer)
            (call-next-method)))

(defun vertex-length (array-vertices)
  (/ (length (buffer-original (buffer array-vertices)))
     (count-attributes (attributes array-vertices))))

(defmethod draw ((o instanced-vertices))
  #+sbcl ; Out our responsibility.
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (%gl:draw-arrays-instanced (draw-mode o) 0 (vertex-length o)
                             (array-dimension
                               (buffer-original (cdar (table o))) 0)))

(define-condition missing-instanced-vertices (fude-gl-error cell-error)
  ((vertices :initarg :vertices :reader vertices))
  (:report
   (lambda (this output)
     (format output "Missing instanced vertices named ~S. ~:@_"
             (cell-error-name this))
     (did-you-mean output (symbol-name (cell-error-name this))
                   (mapcar #'car
                           (table
                             (find-vertices (vertices this)
                                            :if-does-not-exist nil))))
     (format output " ~:@_To see all instanced vertices, evaluate ~S."
             `(table
                (find-vertices ',(vertices this) :if-does-not-exist nil))))))

(define-compiler-macro instances-buffer
                       (&whole whole vertices name &environment env)
  ;; Compile time checking.
  (when (constantp vertices env)
    ;; Is VERTICES defined?
    (let ((instance (find-vertices (eval vertices) :if-does-not-exist nil)))
      (when (constantp name env)
        ;; Is attribute NAME exist?
        (assert (assoc (the symbol (eval name)) (table instance)) ()
          'missing-instanced-vertices :name (eval name)
                                      :vertices (eval vertices)))))
  whole)

(declaim
 (ftype (function (symbol symbol) (values (or null buffer) &optional))
        instances-buffer))

(defun instances-buffer (vertices name)
  (or (cdr
        (assoc name
               (table (find-vertices vertices :if-does-not-exist :create))))
      (error 'missing-instanced-vertices :name name :vertices vertices)))

(defmethod send
           ((o symbol) (vertices-name symbol) &key (method #'gl:buffer-data))
  (let ((buffer (instances-buffer vertices-name o)))
    (send (buffer-source buffer) buffer :method method)))

;; SCENE

(defclass scene (vertices)
  ((file :initarg :file :reader file)
   (scene :initarg :object :reader scene)
   (vertices :initform (make-hash-table :test #'eq) :reader vertices)
   (textures :initform (make-hash-table :test #'equal) :reader textures)))

(defmethod initialize-instance :after
           ((o scene)
            &key (file (error ":FILE is required.")) processing-flags
            &allow-other-keys)
  (setf (slot-value o 'scene)
          (ai:import-into-lisp (uiop:native-namestring file)
                               :processing-flags processing-flags)))

(defun transform-point (vec mat)
  (3d-vectors:v* (apply #'3d-vectors:vec3 (butlast (3d-matrices:mdiag mat)))
                 (3d-vectors:v+ vec
                                (3d-vectors:vxyz (3d-matrices:mcol mat 3)))))

(defun scene-bounds (scene)
  (let ((min (3d-vectors:vec3 1.0e10 1.0e10 1.0e10))
        (max (3d-vectors:vec3 -1.0e10 -1.0e10 -1.0e10)))
    (labels ((mesh-bounds (mesh xform)
               #+sbcl ; due to upgraded element-type is unknown.
               (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
               (loop :for vertex :across (ai:vertices mesh)
                     :for transformed
                          = (transform-point
                              (3d-vectors:vec-from-vector vertex)
                              (3d-matrices:mtranspose xform))
                     :do (setf min (3d-vectors:vmin min transformed)
                               max (3d-vectors:vmax max transformed))))
             (node-bounds (node xform)
               (let ((transform
                      (3d-matrices:nm* (3d-matrices::%mat4 (ai:transform node))
                                       xform)))
                 #+sbcl ; due to upgraded element-type is unknown.
                 (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
                 (loop :for i :across (ai:meshes node)
                       :do (mesh-bounds (aref (ai:meshes scene) i) xform))
                 (loop :for i :across (ai:children node)
                       :do (node-bounds i transform)))))
      (node-bounds (ai:root-node scene) (3d-matrices:meye 4)))
    (values min max)))

(defun model<-scene (scene &key (angle 0))
  (multiple-value-bind (min max)
      (scene-bounds (scene scene))
    (let* ((diff (3d-vectors:v- max min))
           (scale
            (/ 1
               (max (3d-vectors:vx diff) (3d-vectors:vy diff)
                    (3d-vectors:vz diff))))
           (c (3d-vectors:v/ (3d-vectors:v+ min max) 2)))
      (3d-matrices:nmscale
        (3d-matrices:nmrotate
          (3d-matrices:mtranslation (3d-vectors:vapplyf c -)) 3d-vectors:+vz+
          angle)
        (3d-vectors:vec scale scale scale)))))

(defun mesh-types (mesh)
  ;; YAGNI style.
  (case (classimp:primitive-types mesh)
    (1 '(:points))
    (2 '(:lines))
    (4 '(:triangles))
    (otherwise (error "NIY"))))

(defmacro do-node ((var <scene> &optional <return>) &body body)
  (let ((?rec (gensym "REC")))
    (multiple-value-bind (forms decls)
        (uiop:parse-body body)
      `(block nil
         (labels ((,?rec (,var)
                    #+sbcl ; due to upgrade element type is unknown.
                    (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
                    ,@decls
                    (tagbody ,@forms)
                    (map nil #',?rec (ai:children ,var))))
           (,?rec (classimp:root-node ,<scene>))
           (let ((,var))
             (declare (ignorable ,var))
             ,<return>))))))

(defmacro do-mesh ((var <scene> &optional <return>) &body body)
  (let ((?mesh-id (gensym "MESH-ID"))
        (?node (gensym "NODE"))
        (?scene (gensym "SCENE")))
    `(let ((,?scene ,<scene>))
       (do-node (,?node ,?scene)
         #+sbcl ; due to upgrade element type is unknown.
         (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
         (loop :for ,?mesh-id :across (classimp:meshes ,?node)
               :for ,var = (aref (classimp:meshes ,?scene) ,?mesh-id)
               :do (locally ,@body)))
       (let ((,var))
         (declare (ignore ,var))
         ,<return>))))

(defmacro do-material ((var <scene> &optional <return>) &body body)
  `(locally
    #+sbcl
    (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (loop :for ,var :across (ai:materials (scene ,<scene>))
          :do (locally ,@body)
          :finally (return
                    (let ((,var))
                      (declare (ignorable ,var))
                      ,<return>)))))

(defmethod construct ((o scene))
  (do-material (material o)
    (loop :for (semantic tex-index file-name) :in (gethash "$tex.file" material)
          :with counter = (make-hash-table)
          :for uniform
               = (change-case:camel-case
                   (format nil "~A~D"
                           (ecase semantic
                             (:ai-texture-type-height :texture-height)
                             (:ai-texture-type-specular :texture-specular)
                             (:ai-texture-type-diffuse :texture-diffuse))
                           (incf (gethash semantic counter 0))))
          :when (find uniform (uniforms (shader o))
                      :test #'equal
                      :key #'uniform-name)
            :do (setf (gethash uniform (textures o))
                        (construct
                          (make-texture :target :texture-2d
                                        :initializer (lambda ()
                                                       (tex-image-2d
                                                         (load-image
                                                           (merge-pathnames
                                                             file-name
                                                             (file o))
                                                           :flip-y t)))
                                        :params (list :texture-min-filter :linear
                                                      :texture-mag-filter :linear))))))
  (do-mesh (mesh (scene o) o)
    (setf (gethash mesh (vertices o))
            (construct
              (make-instance 'indexed-vertices
                             :array (coerce
                                      (uiop:while-collecting (acc)
                                        (map nil
                                             (lambda
                                                 (vertices normal
                                                  texture-coords)
                                               (map nil #'acc vertices)
                                               (map nil #'acc normal)
                                               (map nil #'acc texture-coords))
                                             (ai:vertices mesh)
                                             (ai:normals mesh)
                                             (aref (ai:texture-coords mesh)
                                                   0)))
                                      '(simple-array single-float (*)))
                             :draw-mode (car (mesh-types mesh))
                             :attributes (mapcar #'class-name (attributes o))
                             :shader (shader o)
                             :indices (list
                                        (uiop:while-collecting (acc)
                                          (map nil
                                               (lambda (indices)
                                                 (map nil #'acc indices))
                                               (ai:faces mesh)))))))))

(defmethod constructed-p ((o scene))
  (and (< 0 (hash-table-count (vertices o)))
       (loop :for array-vertices :being :each :hash-value :of (vertices o)
             :always (constructed-p array-vertices))))

(defmethod destruct ((o scene))
  (loop :for array-vertices :being :each :hash-value :of (vertices o)
        :do (destruct array-vertices))
  (loop :for texture :being :each :hash-value :of (textures o)
        :do (destruct texture)))

(defmethod draw ((o scene))
  (maphash
    (lambda (uniform texture)
      (handler-case (send texture (shader o) :uniform uniform)
        (missing-uniform ()
          #|do-nothing|#)))
    (textures o))
  (do-node (n (scene o))
    (map nil
         (lambda (index)
           (draw (gethash (aref (ai:meshes (scene o)) index) (vertices o))))
         (ai:meshes n))))

;;;; DEFVERTICES

(defmacro defvertices (&whole whole name array &rest options)
  (check-bnf:check-bnf (:whole whole)
    ((name symbol))
    (((vertex-array array) check-bnf:expression))
    (((option* options) option-keys check-bnf:expression)
     (option-keys
      (member :shader
              :draw-mode :indices
              :instances :buffer
              :attributes :type))))
  (unless (getf options :shader)
    (assert (find-shader name)))
  (let ((draw-mode (getf options :draw-mode)))
    (when draw-mode
      (check-type draw-mode draw-mode)))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name *vertices*)
             ,(cond
                ((let ((type (getf options :type))
                       (?class-name (gensym "TYPE"))
                       (?class-options (gensym "CLASS-OPTIONS")))
                   (when type
                     `(destructuring-bind
                          (,?class-name . ,?class-options)
                          ,type
                        (apply #'make-instance ,?class-name :name ',name :file
                               ,array ,@options ,?class-options)))))
                ((getf options :indices)
                 `(make-instance 'indexed-vertices :name ',name :array ,array
                                 ,@options))
                ((getf options :instances)
                 `(make-instance 'instanced-vertices :name ',name :array ,array
                                 ,@options))
                (t
                 `(make-instance 'array-vertices :name ',name :array ,array
                                 ,@options))))
     ',name))

(defmethod documentation ((this (eql 'defvertices)) (type (eql 'function)))
  "Define lisp side vertices object.
(defvertices NAME ARRAY { option* })

  NAME := SYMBOL, names vertices.

  ARRAY := An expression that generates (VECTOR SINGLE-FLOAT).
  This represents vertices and will be send to GL.

  OPTION := [ :shader shader-name
            | :draw-mode DRAW-MODE
            | :indices indices-option
            | :instances instances-option
            | :attributes attributes-option
            | :buffer buffer-option ]
    shader-name := An expression that generates a shader name. See DEFSHADER.
    indices-option := An expression that generates a LIST.
                      The first element of the list is a LIST which have indices.
                      The rest elements of the list is key-value pair for MAKE-BUFFER.
    instances-option := An expression that generates a LIST of instances-clause.
                        The first element of instances-clause is a SYMBOL that is an ATTRIBUTE.
                        The second element of instances-clause is an ARRAY.
                        The rest elements of instances-clause is buffer-option.
    attributes-option := An expression that generates a LIST of attribute names.
    buffer-option := An expression that generates a PLIST.
                     Each key of the PLIST is (member :buffer-usage :buffer-target).
                     Each value of the PLIST is a type of buffer-usage or buffer-taget respectively.")

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
  "Ensure cleanup fude-gl environment."
  (let ((toplevelp (gensym "TOPLEVELP")))
    `(let ((,toplevelp *toplevel-p*) (*toplevel-p* nil))
       (unwind-protect (progn ,@body)
         (when ,toplevelp
           (protect
            (loop :for shader :being :each :hash-value :of *vertices*
                  :when (constructed-p shader)
                    :do (destruct shader)))
           (protect
            (loop :for framebuffer :being :each :hash-value :of *framebuffers*
                  :do (destruct framebuffer)))
           (protect
            (loop :for texture :being :each :hash-value :of *textures*
                  :when (texture-id texture)
                    :do (destruct texture)))
           (protect
            (loop :for name :being :each :hash-key :of *shaders* :using
                       (:hash-value shader)
                  :when (program-id shader)
                    :do (destruct shader))))))))

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

;;; TEXTURE

(defstruct texture
  (id nil :type (or null (unsigned-byte 32)))
  (params nil :type list :read-only t)
  (target (alexandria:required-argument :target)
          :type texture-target
          :read-only t)
  (initializer (alexandria:required-argument :initializer)
               :type function
               :read-only t))

(defvar *textures* (make-hash-table :test #'eq))

(defun list-all-textures ()
  (loop :for name :being :each :hash-key :of *textures*
        :collect name))

(define-condition missing-texture (fude-gl-error cell-error)
  ()
  (:report
   (lambda (this output)
     (format output "Missing texture named ~S. ~:@_" (cell-error-name this))
     (did-you-mean output (symbol-name (cell-error-name this))
                   (list-all-textures))
     (format output " ~:@_To see all known textures, evaluate ~S."
             '(list-all-textures)))))

(define-compiler-macro find-texture (&whole whole name &rest args)
  (declare (ignore args)
           (notinline find-texture))
  (when (constantp name)
    (find-texture (eval name) :if-does-not-exist nil))
  whole)

(defun find-texture (name &key (if-does-not-exist :error))
  (if (typep name 'texture)
      name
      (let ((texture (gethash name *textures*)))
        (cond ;; Texture is not defined yet.
              ((null texture) (error 'missing-texture :name name))
              ;; Texture is constructed in GL already.
              ((texture-id texture) texture)
              ;; Texture is not constructed in GL yet.
              (t
               (ecase if-does-not-exist
                 (:error
                  (cerror "Return texture without constructing."
                          "Texture ~S is not constructed in GL yet." name)
                  texture)
                 (:create
                  (restart-case (construct texture)
                    (continue ()
                        :report "Return texture without constructing."
                      texture)))
                 ((nil) nil)))))))

(defmethod construct ((o texture))
  (with-slots (id target params initializer)
      o
    (unless id
      (setf id (car (gl:gen-textures 1)))
      (gl:bind-texture target id)
      (loop :for (k v) :on params :by #'cddr
            :do (gl:tex-parameter target k v))
      (funcall (the function initializer))))
  o)

(defmethod destruct ((o texture))
  (when (texture-id o)
    (gl:delete-textures (list (texture-id o)))
    (setf (texture-id o) nil)))

(defun unit (shader name)
  (or (loop :for uniform :in (uniforms shader)
            :with unit :of-type fixnum = 0
            :if (equal name (uniform-name uniform))
              :return unit
            :else :if (eq :|sampler2D| (uniform-glsl-type uniform))
              :do (incf unit))
      (error 'missing-uniform :name name :shader shader)))

(defmethod send
           ((o texture) (to symbol)
            &key (uniform (alexandria:required-argument :uniform))
            (unit (unit to uniform)))
  (in-program to)
  (gl:uniformi (uniform to uniform) unit)
  (gl:active-texture unit)
  (gl:bind-texture (texture-target o) (texture-id o)))

(defun connect (shader &rest pairs)
  (loop :for i :of-type (mod #.most-positive-fixnum) :upfrom 0
        :for (uniform name) :on pairs :by #'cddr
        :do (send (find-texture name :if-does-not-exist :create) shader
                  :uniform uniform
                  :unit i)))

(defun type-assert (form type)
  (if (constantp form)
      (locally
       #+sbcl ; due to unknown TYPE at compile time.
       (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
       (assert (typep form type) ()
         "~S is not type of ~S" form (millet:type-expand type))
       form)
      `(the ,type ,form)))

(defmacro deftexture (name target init-form &body options)
  (type-assert target 'texture-target)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ;; We need this EVAL-WHEN for compile time checkings.
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

(defmacro in-texture (name)
  (when (constantp name)
    (find-texture (eval name) :if-does-not-exist nil))
  `(let ((texture (find-texture ,name :if-does-not-exist :create)))
     (gl:bind-texture (texture-target texture) (texture-id texture))))

;;;; WITH-DELTA-TIME

(locally
 #+sbcl ; due to not fixnum
 (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
 (defstruct (delta-time (:constructor make-delta-time
                         (&aux (last-frame (get-internal-real-time))
                          (delta (- (get-internal-real-time) last-frame)))))
   (last-frame 0 :type (integer 0 *))
   (delta 0 :type (unsigned-byte 32))))

(defun update-delta-time (time)
  #+sbcl ; due to not fixnum
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((current-frame (get-internal-real-time)))
    (with-slots (last-frame delta)
        time
      (setf delta (- current-frame last-frame)
            last-frame current-frame))
    time))

(defvar *delta*)

(defmacro with-delta-time ((var) &body body)
  `(progn (setq *delta* (delta-time-delta (update-delta-time ,var))) ,@body))

;;;; WITH-CLEAR

(defmacro with-clear
          (&whole whole (var-win (&rest buf*) &key (color ''(0.0 0.0 0.0 1.0)))
           &body body)
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (check-bnf:check-bnf (:whole whole)
    ((var-win symbol))
    ((buf* buffer-bit))
    ((color check-bnf:expression)))
  `(progn
    (apply #'gl:clear-color ,color)
    (gl:clear ,@(mapcar (lambda (buf) (type-assert buf 'buffer-bit)) buf*))
    ,@body
    (sdl2:gl-swap-window ,var-win)))

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

(set-pprint-dispatch '(cons (member with-clear with-framebuffer))
                     'pprint-with-clear)

;;;; FRAMEBUFFER

(defun default-renderbuffer-initializer (framebuffer)
  (with-slots (render-buffer width height)
      framebuffer
    (gl:bind-renderbuffer :renderbuffer render-buffer)
    (gl:renderbuffer-storage :renderbuffer :depth24-stencil8 width height)
    (gl:framebuffer-renderbuffer :framebuffer :depth-stencil-attachment
                                 :renderbuffer render-buffer)
    (gl:bind-renderbuffer :renderbuffer 0))
  (values))

(defclass framebuffer ()
  ((id :initform nil :type (or null (unsigned-byte 32)) :reader framebuffer-id)
   (texture :initform nil :reader framebuffer-texture)
   (render-buffer :initform nil)
   (format :initarg :format :initform :rgb :type base-internal-format)
   (pixel-type :initarg :pixel-type :initform :unsigned-byte :type pixel-type)
   (options :initarg :options :initform nil :type list)
   (attachment :initarg :attachment
               :initform :color-attachment0
               :type attachment)
   (renderbuffer-initializer :initarg :renderbuffer-initializer
                             :initform #'default-renderbuffer-initializer
                             :type function)
   (width :initarg :width
          :initform (alexandria:required-argument :width)
          :type (mod #.most-positive-fixnum))
   (height :initarg :height
           :initform (alexandria:required-argument :height)
           :type (mod #.most-positive-fixnum))))

(defmethod framebuffer-texture ((o symbol))
  (framebuffer-texture (find-framebuffer o :if-does-not-exist :create)))

(defvar *framebuffers* (make-hash-table :test #'eq))

(defun list-all-framebuffers () (alexandria:hash-table-keys *framebuffers*))

(define-condition missing-framebuffer (fude-gl-error cell-error)
  ()
  (:report
   (lambda (this output)
     (format output "Missing framebuffer named ~S. ~:@_"
             (cell-error-name this))
     (did-you-mean output (symbol-name (cell-error-name this))
                   (list-all-framebuffers))
     (format output " ~:@_To see all known framebuffer, evaluate ~S."
             '(list-all-framebuffers)))))

(defun find-framebuffer (name &key (if-does-not-exist :error))
  (let ((framebuffer (gethash name *framebuffers*)))
    (assert framebuffer () 'missing-framebuffer :name name)
    (if (framebuffer-id framebuffer)
        framebuffer
        (ecase if-does-not-exist
          (:error
           (cerror "Return framebuffer without constructing."
                   "Framebuffer ~S is not constructed in openGL yet." name)
           framebuffer)
          (:create
           (restart-case (construct framebuffer)
             (continue ()
                 :report "Return framebuffer without constructing."
               framebuffer)))
          ((nil) framebuffer)))))

(defmethod construct ((o framebuffer))
  (with-slots (id texture render-buffer format width height pixel-type options
               attachment renderbuffer-initializer)
      o
    (declare (function renderbuffer-initializer))
    (setf id (car (gl:gen-framebuffers 1))
          texture
            (construct
              (make-texture :target :texture-2d
                            :params (loop :with default
                                                = (list :texture-min-filter :linear
                                                        :texture-mag-filter :linear)
                                          :for (k v) :on options :by #'cddr
                                          :do (setf (getf default k) v)
                                          :finally (return default))
                            :initializer (lambda ()
                                           #+sbcl ; due to not our responsibility.
                                           (declare
                                            (sb-ext:muffle-conditions
                                             sb-ext:compiler-note))
                                           (gl:tex-image-2d :texture-2d 0
                                                            format width height
                                                            0 format pixel-type
                                                            (cffi:null-pointer)))))
          render-buffer (car (gl:gen-renderbuffers 1)))
    (gl:bind-framebuffer :framebuffer id)
    (framebuffer-texture-2d :framebuffer attachment (texture-target texture)
                            (texture-id texture) 0)
    (funcall renderbuffer-initializer o)
    (let ((result (gl:check-framebuffer-status :framebuffer)))
      (unless (find result
                    '(:framebuffer-complete :framebuffer-complete-ext
                      :framebuffer-complete-oes))
        (error "Fails to initialize framebuffer. ~S" result)))
    (gl:bind-framebuffer :framebuffer 0))
  o)

(defmethod destruct ((o framebuffer))
  (with-slots (id texture render-buffer)
      o
    (when id
      (gl:delete-framebuffers (list id))
      (setf id nil))
    (when texture
      (destruct texture))
    (when render-buffer
      (gl:delete-renderbuffers (list render-buffer))
      (setf render-buffer nil))))

(defun in-framebuffer (name)
  (gl:bind-framebuffer :framebuffer (if name
                                        (framebuffer-id
                                          (find-framebuffer name
                                                            :if-does-not-exist :create))
                                        0)))

(defvar *framebuffer-toplevelp* t)

(defmacro with-framebuffer
          ((name
            (&rest buffers)
            &key
            (color ''(0 0 0 1))
            (win (alexandria:required-argument :win)))
           &body body)
  (check-type name symbol)
  (find-framebuffer name :if-does-not-exist nil)
  `(progn
    (in-framebuffer ',name)
    (with-slots (width height)
        (find-framebuffer ',name :if-does-not-exist :create)
      (gl:viewport 0 0 width height))
    ,@(when color
        `((apply #'gl:clear-color ,color)))
    (gl:clear ,@buffers)
    ,@body
    (in-framebuffer nil)
    (multiple-value-call #'gl:viewport 0 0 (sdl2:get-window-size ,win))))

(defmacro deframebuf (name &rest params)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ;; We need EVAL-WHEN for compile time checkings.
     (setf (gethash ',name *framebuffers*)
             (make-instance 'framebuffer ,@params))))

(defun pprint-deframebuf (stream exp)
  (funcall
    (formatter
     #.(apply #'concatenate 'string
              (alexandria:flatten
                (list "~:<" ; pprint-logical-block.
                      "~W~^ ~1I~@_" ; operator DEFRAMEBUF
                      "~W~^ ~_" ; name
                      "~@{~W~^ ~@_~W~^ ~_~}" ; k-v options.
                      "~:>"))))
    stream exp))

(set-pprint-dispatch '(cons (member deframebuf)) 'pprint-deframebuf)

;;;; CAMERA

(defclass camera ()
  ((position :initform (3d-vectors:vec3 0 0 3)
             :initarg :position
             :type 3d-vectors:vec3
             :reader camera-position)
   (target :initform (3d-vectors:vec3 0 0 0)
           :initarg :target
           :type 3d-vectors:vec3
           :reader camera-target)
   (front :initform (3d-vectors:vec3 0 0 -1)
          :initarg :front
          :type 3d-vectors:vec3
          :reader camera-front)
   ;; NOTE: This slot may not needed.
   ;; Used only in initialize?
   (right :type 3d-vectors:vec3 :reader camera-right)
   (up :type 3d-vectors:vec3 :reader camera-up)))

(defmethod initialize-instance :after ((o camera) &key)
  (with-slots (position target right up)
      o
    (let ((direction (3d-vectors:nvunit (3d-vectors:v- position target))))
      (setf right
              (3d-vectors:nvunit
                (3d-vectors:vc (3d-vectors:vec3 0 1 0) direction))
            up (3d-vectors:vc direction right)))))

(defclass looker (camera)
  ((sensitivity :initform 0.1
                :initarg :sensitivity
                :type single-float
                :accessor sensitivity)
   (sight :initform (3d-vectors:vec3 0 -90 0)
          :initarg :sight
          :type 3d-vectors:vec3
          :accessor sight)
   (last-position :initform (3d-vectors:vec3 0 0 0)
                  :initarg :last-position
                  :type 3d-vectors:vec3
                  :accessor last-position)
   (field-of-view :initform 45
                  :initarg :field-of-view
                  :type (unsigned-byte 16)
                  :accessor field-of-view)))

(defgeneric view (camera &key)
  (:method ((camera camera) &key look-at-target)
    (3d-matrices:mlookat (camera-position camera)
                         (if look-at-target
                             (camera-target camera)
                             (3d-vectors:v+ (camera-position camera)
                                            (camera-front camera)))
                         (camera-up camera))))

(defgeneric move (camera x &rest args)
  (:method ((camera camera) x &rest yz)
    (3d-vectors::%vsetf (camera-position camera) x (car yz) (cadr yz))
    camera)
  (:method ((camera camera) (to 3d-vectors:vec3) &rest noise)
    (declare (ignore noise))
    (3d-vectors:nv+ (camera-position camera) to)
    camera))

(defun pitch (camera) (3d-vectors:vx (sight camera)))

(defun yaw (camera) (3d-vectors:vy (sight camera)))

(defun roll (camera) (3d-vectors:vz (sight camera)))

(defgeneric lookat (camera x y mask)
  (:method ((camera looker) x y mask)
    (declare (ignore mask)
             (type (unsigned-byte 32) x y mask))
    (let* ((sensitivity (the single-float (sensitivity camera)))
           (offset-x
            (* sensitivity
               (- x (shiftf (3d-vectors:vx (last-position camera)) x))))
           ;; reversed. Y ranges bottom to top.
           (offset-y
            (* sensitivity
               (- (shiftf (3d-vectors:vy (last-position camera)) y) y)))
           (pitch (max -89.0 (min 89.0 (+ (pitch camera) offset-y))))
           (yaw (+ (yaw camera) offset-x)))
      (3d-vectors:vsetf (sight camera) pitch yaw)
      (3d-vectors:vsetf (camera-front camera)
                        (* (cos (radians yaw)) (cos (radians pitch)))
                        (sin (radians pitch))
                        (* (sin (radians yaw)) (cos (radians pitch))))
      (3d-vectors:nvunit (camera-front camera)))
    camera))

(defgeneric zoom (camera direction w h)
  (:method ((camera looker) direction w h)
    (declare (type fixnum direction w h))
    (3d-matrices:mperspective
      (setf (field-of-view camera)
              (case direction
                (1 (min 45 (1+ (the fixnum (field-of-view camera)))))
                (-1 (max 1 (1- (the fixnum (field-of-view camera)))))
                (otherwise (field-of-view camera))))
      (/ w h) 0.1 100)))

;; MATRIX

(defun radians (degrees)
  #+sbcl ; Due to unknown type.
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (* degrees (/ pi 180)))

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

(unless (boundp '+meye4+)
  (defconstant +meye4+ (3d-matrices:meye 4)))

(defun reload (dest source)
  (replace (3d-matrices:marr dest) (3d-matrices:marr source))
  dest)

;;;; IMAGE-LOADER
;; *IMAGES*

(defvar *images* (make-hash-table :test #'equal))

(defun list-all-images () (alexandria:hash-table-keys *images*))

(define-compiler-macro image (&whole whole name &environment env)
  (when (constantp name env)
    (assert (gethash (eval name) *images*) ()
      "Unknown image ~S. Eval (list-all-iamges)." (eval name)))
  whole)

(defun image (name)
  (or (gethash name *images*)
      (error "Unknown image ~S. Eval (fude-gl:list-all-images)." name)))

;; LOAD-IMAGE

(defmacro efiletype-case (pathname &body clause*)
  ;; Trivial syntax check.
  (assert (every (lambda (clause) (typep clause '(cons cons *))) clause*))
  (let ((type (gensym "FILETYPE")))
    `(let ((,type (pathname-type ,pathname)))
       (cond
         ,@(mapcar
             (lambda (clause)
               `((find ,type ',(car clause) :test #'string-equal)
                 ,@(cdr clause)))
             clause*)
         (t
          (error "Not supported file type ~S. ~S" ,type
                 ',(loop :for c :in clause*
                         :append (car c))))))))

(defun load-image (filename &key flip-y)
  (flet ((may-flip (image)
           (if flip-y
               (opticl:vertical-flip-image image)
               image)))
    (efiletype-case (truename filename)
      ((png) (may-flip (opticl:read-png-file filename)))
      ((jpg jpeg) (may-flip (opticl:read-jpeg-file filename))))))

;; DEFIMAGE

(defmacro defimage (name pathname &key flip-y)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ;; The compiler macro for IMAGE needs this eval-when.
     (progn
      (setf (gethash ',name *images*) (load-image ,pathname :flip-y ,flip-y))
      ',name)))

(defun pprint-defimage (out exp &rest noise)
  (declare (ignore noise))
  (funcall (formatter "~:<~W~^ ~1I~:_~W~^ ~:_~W~^~_~@{~W~^ ~@_~W~^ ~_~}~:>")
           out exp))

(set-pprint-dispatch '(cons (member defimage)) 'pprint-defimage)