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
     (format output
             "Missing vertices named ~S. ~:@_~? ~:@_To see defined vertices, eval ~S"
             (cell-error-name this)
             "Did you mean ~#[~;~S~;~S or ~S~:;~S, ~S or ~S~] ?"
             (fuzzy-match:fuzzy-match (princ-to-string (cell-error-name this))
                                      (list-all-vertices))
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

(defun tex-image-2d (array)
  #+sbcl ; Due to the array dimensions are unknown in compile time.
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
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

;;;; VERTEX-ATTRIBUTE-CLASSES

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; NOTE: CHECK-BNF in DEFSHADER needs this eval-when.
  ;; Additionally define-vertex-attribute below needs this too.
  (defparameter *vertex-attributes* (make-hash-table))
  (defun vertex-attribute-p (thing)
    (and (symbolp thing) (values (gethash thing *vertex-attributes*)))))

(defun list-all-attributes () (alexandria:hash-table-keys *vertex-attributes*))

;; METACLASS

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; NOTE: DEFINE-VERTEX-ATTRIBUTE below needs this eval-when.
  (defclass vector-class (standard-class)
    ((program-id :accessor program-id :initform nil)))
  (defmethod c2mop:validate-superclass ((c vector-class) (s standard-class)) t)
  ;; METACLASS for attributes.
  (defclass attributes (vector-class) ())
  ;; METACLASS for instanced-array.
  (defclass instanced-array (vector-class) ()))

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
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ;; check-bnf needs this eval-when.
     (defclass ,name ()
       ,(mapcar
          (lambda (slot)
            #+sbcl ; Due to SLOT may symbol or character.
            (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
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
  (:documentation "Return associated uniform name strings."))

(define-condition missing-shader (fude-gl-error cell-error)
  ((interface :initarg :interface :reader interface))
  (:report
   (lambda (this output)
     (format output
             "Missing shader named ~S. ~:@_~? ~:@_To see all known shaders, evaluate ~S."
             (cell-error-name this)
             "Did you mean ~#[~;~S~;~S or ~S~:;~S, ~S or ~S~] ?"
             (fuzzy-match:fuzzy-match (princ-to-string (cell-error-name this))
                                      (loop :for method
                                                 :in (c2mop:generic-function-methods
                                                       (interface this))
                                            :for specializer
                                                 := (car
                                                      (c2mop:method-specializers
                                                        method))
                                            :when (typep specializer
                                                         'c2mop:eql-specializer)
                                              :collect (c2mop:eql-specializer-object
                                                         specializer)))
             `(c2mop:generic-function-methods
                #',(c2mop:generic-function-name (interface this)))))))

(defmethod no-applicable-method ((gf (eql #'vertex-shader)) &rest args)
  (error 'missing-shader :name (car args) :interface gf))

(defmethod no-applicable-method ((gf (eql #'fragment-shader)) &rest args)
  (error 'missing-shader :name (car args) :interface gf))

(defmethod no-applicable-method ((gf (eql #'uniforms)) &rest args)
  (error 'missing-shader :name (car args) :interface gf))

(defgeneric construct (thing)
  (:documentation "Requesting openGL to construct objects."))

(defgeneric destruct (thing)
  ;; NOTE!
  ;; When code fails while constructing,
  ;; DESTRUCT may be called before bound slot.
  (:documentation "Requesting openGL to destruct objects."))

(defgeneric create-vertex-array (vertices))

(define-compiler-macro draw (&whole whole thing)
  (when (constantp thing)
    (handler-case (find-vertices (eval thing) :if-does-not-exist nil)
      (missing-vertices (c)
        (warn 'missing-definition :condition c))))
  whole)

(defgeneric draw (thing) (:documentation "Request openGL to draw THING."))

(defgeneric send (object to &key))

(defstruct uniform
  (name (error "NAME is required.") :type string :read-only t)
  (type (error "TYPE is required.") :type string :read-only t))

(defmethod make-load-form ((this uniform) &optional environment)
  (make-load-form-saving-slots this :environment environment))

(define-compiler-macro shader (&whole whole o)
  (declare (notinline shader))
  (if (constantp o)
      `',(shader (find-vertices (eval o) :if-does-not-exist nil))
      whole))

(define-condition uniform-error (fude-gl-error cell-error)
  ((shader :initarg :shader :reader shader)))

(define-condition missing-uniform (uniform-error)
  ()
  (:report
   (lambda (this output)
     (format output
             "Missing uniform named ~S. ~:@_~? ~:@_To see all supported uniforms, evaluate ~S."
             (cell-error-name this)
             "Did you mean ~#[~;~S~;~S or ~S~:;~S, ~S or ~S~] ?"
             (fuzzy-match:fuzzy-match (cell-error-name this)
                                      (mapcar #'uniform-name
                                              (uniforms (shader this))))
             `(uniforms ',(shader this))))))

(define-condition non-active-uniform (uniform-error)
  ()
  (:report
   (lambda (this output)
     (format output
             "Uniform ~S is not active in shader ~S. ~:@_Active uniforms in GL are ~S."
             (cell-error-name this) (shader this)
             (gl:get-program (program-id (find-shader (shader this)))
                             :active-uniforms)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Compiler macro and setf expander below needs this eval-when.
  (defun check-uniform-args (shader name)
    "Compile time argument validation for UNIFORM."
    (when (constantp shader)
      (let ((shader (eval shader)))
        (find-shader shader)
        (when (constantp name)
          (let ((name (eval name)))
            (declare (string name))
            (assert (find (subseq name 0 (position #\[ name))
                          (the list (uniforms shader))
                          :test #'equal
                          :key #'uniform-name)
              ()
              'missing-uniform :name name
                               :shader shader)))))))

(define-compiler-macro send
                       (&whole whole object to &key uniform &allow-other-keys)
  (declare (ignore object))
  ;; Compile time uniform existance check.
  (and uniform (check-uniform-args to uniform))
  whole)

;;;; DEFSHADER

(defun uniform-keywordp (thing) (and (symbolp thing) (string= '&uniform thing)))

(defun varying-keywordp (thing) (and (symbolp thing) (string= '&varying thing)))

(deftype var () 'symbol)

(deftype type-spec () 'keyword)

(deftype complex-type-spec () 'list)

(deftype index () '(mod #.array-total-size-limit))

(declaim
 (ftype (function
         ((cons var
                (cons (or type-spec complex-type-spec)
                      (or null (cons index null)))))
         (values (cons null (cons string (cons string null))) &optional))
        parse-shader-lambda-list-spec))

(defun parse-shader-lambda-list-spec (spec)
  (destructuring-bind
      (name type . vector-size)
      spec
    (list ;; Layout location, in order to share the formatter with class-shader-inputs.
          nil
          ;; GLSL type string.
          (etypecase type
            (symbol (symbol-camel-case type))
            (list
             (format nil "~:@(~A~) ~?" name
                     #.(concatenate 'string "~:<{~;~3I~:@_" ; pprint-logical-block.
                                    "~@{~A~^ ~@_~A;~^~:@_~}" "~%~;}~:> ")
                     (list
                       (loop :for (name type) :in type
                             :collect (symbol-camel-case type)
                             :collect (symbol-camel-case name))))))
          ;; Var name string.
          (if vector-size
              (format nil "~A[~A]" (symbol-camel-case name) (car vector-size))
              (symbol-camel-case name)))))

(declaim
 (ftype (function (list) (values list list list list &optional))
        split-shader-lambda-list))

(defun split-shader-lambda-list (lambda-list)
  (uiop:while-collecting (out uniform varying vars)
    (loop :with collector = #'out
          :for elt :in lambda-list
          :if (uniform-keywordp elt)
            :do (setf collector #'uniform)
          :else :if (varying-keywordp elt)
            :do (setf collector #'varying)
          :else
            :do (vars (car elt))
                (mapc collector (parse-shader-lambda-list-spec elt)))))

(defun <uniforms> (name shader*)
  ;; Shader NAME becomes VECTOR-CLASS.
  ;; MAKE-INSTANCE for VECTOR-CLASS makes a VECTOR instead of an object instance.
  ;; So we should use eql-specializer instead of class.
  `(defmethod uniforms ((type (eql ',name)))
     (list
       ,@(loop :for (nil lambda-list) :in shader*
               :for uniforms
                    = (nth-value 1 (split-shader-lambda-list lambda-list))
               :nconc (loop :for (nil type name) :on uniforms :by #'cdddr
                            :collect (make-uniform :name name :type type))))))

(defun class-shader-inputs (superclasses)
  (loop :for c :in (mapcar #'find-class superclasses)
        :for slots = (c2mop:class-direct-slots c)
        :for i :of-type (mod #.most-positive-fixnum) :upfrom 0
        :when slots
          :collect (format nil "layout (location = ~A) " i)
          :and :collect (format nil "~[~;float~:;~:*vec~D~]"
                                (list-length slots))
          :and :collect (symbol-camel-case (class-name c))))

(defun <shader-forms> (shader-clause* superclasses name version)
  (let ((format
         (formatter
          #.(concatenate 'string "#version ~A core~%" ; version
                         "~{~@[~A~]in ~A ~A;~%~}~&" ; in
                         "~{out ~A ~A;~%~}~&" ; out
                         "~@[~{uniform ~A ~A;~%~}~]~&" ; uniforms
                         "~@[~{varying ~A ~A;~%~}~]~&" ; varying.
                         "~@[~{~/fude-gl:pprint-glsl/~^~}~]" ; functions.
                         )))
        (*shader-vars* (uiop:list-to-hash-set superclasses)))
    (labels ((rec (shaders in varying acc)
               (if (endp shaders)
                   (nreverse acc)
                   (body (car shaders) (cdr shaders) in varying acc)))
             (body (shader rest in varying acc)
               (destructuring-bind
                   (type shader-lambda-list &rest main)
                   shader
                 (multiple-value-bind (out uniform varying% vars)
                     (split-shader-lambda-list shader-lambda-list)
                   (dolist (var vars) (setf (gethash var *shader-vars*) t))
                   (rec rest out (append varying varying%)
                        (cons
                          (let ((method
                                 (intern (format nil "~A-SHADER" type)
                                         :fude-gl)))
                            `(defmethod ,method ((type (eql ',name)))
                               ,(if (typep main
                                           '(cons
                                              (cons (eql quote)
                                                    (cons symbol null))
                                              null))
                                    `(,method ',(cadar main))
                                    (format nil format version in
                                            (remove nil out)
                                            (delete nil uniform)
                                            (remove nil
                                                    (append varying varying%))
                                            main))))
                          acc))))))
      (rec shader-clause* (class-shader-inputs superclasses) nil nil))))

(defvar *shaders*
  (make-hash-table :test #'eq)
  "Repository of the meta shader objects.")

(define-condition missing-program (fude-gl-error cell-error)
  ()
  (:report
   (lambda (this output)
     (format output "Shader program named ~S is not created in GL yet."
             (cell-error-name this)))))

(defun find-shader (name &optional (errorp t))
  (or (gethash name *shaders*)
      (and errorp (error 'missing-program :name name))))

(defmacro defshader
          (&whole whole name version (&rest attribute*) &body shader*)
  (check-bnf:check-bnf (:whole whole)
    ((name symbol))
    ((version (mod #.most-positive-fixnum)))
    ((attribute* (satisfies vertex-attribute-p)))
    ((shader* (or vertex-clause fragment-clause))
     ;;
     (vertex-clause ((eql :vertex) shader-lambda-list main*))
     ;;
     (fragment-clause ((eql :fragment) shader-lambda-list main*))
     ;;
     (shader-lambda-list
      (out-spec* uniform-keyword? uniform-spec* varying-keyword?
       varying-spec*))
     (out-spec (var (or type-spec complex-type)))
     (uniform-keyword (satisfies uniform-keywordp))
     (uniform-spec (var type-spec vector-size?))
     (vector-size (mod #.array-total-size-limit))
     (varying-keyword? (satisfies varying-keywordp))
     (varying-spec (var type-spec))
     ;;
     (complex-type out-spec*)
     (main check-bnf:expression)))
  ;; The body.
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name *shaders*)
             (defclass ,name ,attribute* () (:metaclass vector-class)))
     ,@(<shader-forms> shader* attribute* name version)
     ,(<uniforms> name shader*)
     ',name))

(defmethod documentation ((this (eql 'defshader)) (type (eql 'function)))
  "Define lisp side meta shader protocol object and its generic function methods.
You can define GLSL vertex shader code generator (i.e. generic-function VERTEX-SHADER) and/or
GLSL fragment shader code generator (i.e. generic-function FRAGMENT-SHADER) and
vertex constructor (i.e. make-instance NAME.) via this macro.
Vertex constructor makes a single-float vector that's length depends on its ATTRIBUTEs.")

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
                                  "~@{~/fude-gl::pprint-shader-clause-body/~^ ~_~}" ; clause
                                                                                    ; body.
                                  "~:>~^ ~_")
                            "~}")
                      "~:>"))))
    stream exp))

(defun pprint-shader-clause-body (stream exp &rest noise)
  (declare (ignore noise))
  (if (not (typep exp '(cons (member flet))))
      (write exp :stream stream)
      (funcall
        (formatter
         #.(apply #'concatenate 'string
                  (alexandria:flatten
                    (list "~:<" ; pprint-logical-block
                          "~W~^~1I ~@_" ; operator FLET.
                          "~W~^ ~@_" ; return type.
                          "~W~^ ~@_" ; function name.
                          (list "~:<" ; logical block for lambda list.
                                "~@{~W~^ ~:_~}" ; lambda list elts.
                                "~:>~^ ~_")
                          "~@{~W~^ ~_~}" ; clause body.
                          "~:>"))))
        stream exp)))

(set-pprint-dispatch '(cons (member defshader)) 'pprint-defshader)

(defun compile-shader (program-id vertex-shader fragment-shader)
  "Request openGL to compile and link shader programs. Return nil."
  (let ((vs (gl:create-shader :vertex-shader))
        (fs (gl:create-shader :fragment-shader)))
    (unwind-protect
        (labels ((compile-s (program-id id source)
                   (gl:shader-source id source)
                   (gl:compile-shader id)
                   (may-warn (gl:get-shader-info-log id))
                   (gl:attach-shader program-id id))
                 (may-warn (log)
                   (unless (equal "" log)
                     (warn log))))
          (compile-s program-id vs vertex-shader)
          (compile-s program-id fs fragment-shader)
          (gl:link-program program-id)
          (may-warn (gl:get-program-info-log program-id)))
      (gl:delete-shader fs)
      (gl:delete-shader vs))))

(define-condition fail-to-create-program (fude-gl-error cell-error)
  ()
  (:report
   (lambda (this output)
     (format output
             "Fails to create program named ~S. ~:_Is opengl context achieved?"
             (cell-error-name this)))))

(defun create-program (shader)
  "Request openGL to create SHADER, return its ID."
  (let ((program (the (unsigned-byte 32) (gl:create-program))))
    (when (zerop program)
      (error 'fail-to-create-program :name shader))
    (compile-shader program (vertex-shader shader) (fragment-shader shader))
    program))

(defun find-program (name &key (if-does-not-exist :error))
  "Return program ID of the NAME if exists, otherwise depends on IF-DOES-NOT-EXIST."
  (let ((instance (find-shader name nil)))
    (or (and instance (program-id instance))
        (ecase if-does-not-exist
          (:error (error 'missing-program :name name))
          (:create (program-id (construct (find-shader name))))
          ((nil) nil)))))

(defmethod construct ((o vector-class))
  (setf (program-id o) (create-program (class-name o)))
  o)

(defmethod destruct ((o vector-class))
  (gl:delete-program (program-id o))
  (setf (program-id o) nil))

(defmacro in-program (name)
  (when (constantp name)
    (find-shader (eval name)))
  `(gl:use-program (find-program ,name :if-does-not-exist :create)))

;;;; UNIFORM

(define-compiler-macro uniform (&whole whole shader name)
  (check-uniform-args shader name)
  whole)

(defun uniform (shader name)
  (let ((location
         (gl:get-uniform-location (program-id (find-shader shader)) name)))
    (declare ((signed-byte 32) location))
    (when (minusp location)
      (if (find name (the list (uniforms shader))
                :test #'equal
                :key #'uniform-name)
          (error 'non-active-uniform :name name :shader shader)
          (error 'missing-uniform :name name :shader shader)))
    location))

(define-setf-expander uniform (shader name &rest args)
  (check-uniform-args shader name)
  (let ((new (gensym "NEW")))
    (values nil
            nil
            (list new)
            `(progn (send ,new ,shader :uniform ,name ,@args) ,new))))

(defmacro with-uniforms ((&rest var*) shader &body body)
  ;; Trivial-syntax-check
  (when (constantp shader)
    (dolist (var var*)
      (let ((name
             (if (symbolp var)
                 (symbol-camel-case var)
                 (if (stringp (cadr var))
                     (cadr var)
                     (symbol-camel-case (car var))))))
        (check-uniform-args shader name))))
  (let ((s (gensym "SHADER")))
    `(let ((,s ,shader))
       (symbol-macrolet ,(loop :for spec :in var*
                               :if (symbolp spec)
                                 :collect `(,spec
                                            (uniform ,s
                                                     ,(symbol-camel-case spec)))
                               :else :if (stringp (cadr spec))
                                 :collect `(,(car spec)
                                            (uniform ,s ,@(cdr spec)))
                               :else
                                 :collect `(,(car spec)
                                            (uniform ,s
                                                     ,(symbol-camel-case
                                                        (car spec))
                                                     ,@(cdr spec))))
         ,@body))))

(defmethod send
           ((o integer) (to symbol)
            &key (uniform (alexandria:required-argument :uniform)))
  (in-program to)
  (gl:uniformi (uniform to uniform) o))

(defmethod send
           ((o float) (to symbol)
            &key (uniform (alexandria:required-argument :uniform)))
  #+sbcl ; Due to unknown single float or double float.
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (in-program to)
  (gl:uniformf (uniform to uniform) o))

(defmethod send
           ((o 3d-matrices:mat4) (to symbol)
            &key (uniform (alexandria:required-argument :uniform)))
  (in-program to)
  (gl:uniform-matrix (uniform to uniform) 4 (vector (3d-matrices:marr o))))

(defmethod send
           ((o 3d-vectors:vec3) (to symbol)
            &key (uniform (alexandria:required-argument :uniform)))
  (in-program to)
  (3d-vectors:with-vec3 (x y z)
      o
    (gl:uniformf (uniform to uniform) x y z)))

(defmethod send
           ((o 3d-vectors:vec2) (to symbol)
            &key (uniform (alexandria:required-argument :uniform)))
  (in-program to)
  (3d-vectors:with-vec2 (x y)
      o
    (gl:uniformf (uniform to uniform) x y)))

(defmethod send
           ((o vector) (to symbol)
            &key (uniform (alexandria:required-argument :uniform)))
  #+sbcl ; Due to unknown upgraded element type.
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (in-program to)
  (gl:uniformfv (uniform to uniform) o))

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
  ((shader :type symbol :reader shader)
   (buffer :type buffer :reader buffer)
   (attributes :type list :reader attributes)
   (draw-mode :type draw-mode :reader draw-mode)
   (vertex-array :type (unsigned-byte 32) :reader vertex-array)))

(defmethod initialize-instance :after
           ((o vertices)
            &key name buffer array shader attributes (draw-mode :triangles))
  (setf (slot-value o 'shader) (or shader name)
        (slot-value o 'buffer)
          (apply #'make-buffer :name :vertices :original array buffer)
        (slot-value o 'draw-mode) draw-mode
        (slot-value o 'attributes)
          (or (mapcar #'find-class attributes)
              (c2mop:class-direct-superclasses (find-class (or shader name))))))

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
          ((slot-boundp vertices 'vertex-array) vertices)
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

(defmethod create-vertex-array :around ((o vertices))
  (let ((vao (gl:gen-vertex-array)))
    (in-vertex-array vao)
    (call-next-method)
    vao))

(defmethod create-vertex-array ((o vertices))
  (link-attributes (attributes o) (constantly (buffer o))))

(defmethod construct ((o vertices))
  (with-slots (vertex-array shader)
      o
    (find-program shader :if-does-not-exist :create)
    (setf vertex-array (create-vertex-array o))
    o))

(defmethod destruct ((o vertices))
  (when (slot-boundp o 'vertex-array)
    (gl:delete-vertex-arrays (list (vertex-array o)))
    (slot-makunbound o 'vertex-array))
  (destruct (buffer o)))

(defmethod send ((o (eql :buffer)) (to symbol) &key (method #'gl:buffer-data))
  (let ((buffer (buffer (find-vertices to :if-does-not-exist :create))))
    (send (buffer-source buffer) buffer :method method)))

(defmethod draw ((name symbol))
  (draw (find-vertices name :if-does-not-exist :create)))

(defmethod draw :before ((o vertices)) (in-vertices o))

(defmethod draw ((o vertices))
  #+sbcl ; Due to out of our responsibility.
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (gl:draw-arrays (draw-mode o) 0 (vertex-length o)))

;; INDEXED

(defclass indexed-vertices (vertices) ((indices :type buffer :reader indices)))

(defmethod initialize-instance :after
           ((o indexed-vertices) &key indices &allow-other-keys)
  (setf (slot-value o 'indices)
          (apply #'make-buffer :name :indices :original
                 (coerce (the list (car indices))
                         '(array (unsigned-byte 8) (*)))
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

(defclass instanced-vertices (vertices instanced) ())

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

(defun vertex-length (vertices)
  (/ (length (buffer-original (buffer vertices)))
     (count-attributes (attributes vertices))))

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
     (format output
             "Missing instanced vertices named ~S. ~:@_~? ~:@_To see all instanced vertices, evaluate ~S."
             (cell-error-name this)
             "Did you mean ~#[~;~S~;~S or ~S~:;~S, ~S or ~S~] ?"
             (fuzzy-match:fuzzy-match (symbol-name (cell-error-name this))
                                      (mapcar #'car
                                              (table
                                                (find-vertices (vertices this)
                                                               :if-does-not-exist nil))))
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

;;;; DEFVERTICES

(defmacro defvertices (&whole whole name array &rest options)
  (check-bnf:check-bnf (:whole whole)
    ((name symbol))
    (((vertex-array array) check-bnf:expression))
    (((option* options) option-keys check-bnf:expression)
     (option-keys
      (member :shader :draw-mode :indices :instances :buffer :attributes))))
  (unless (getf options :shader)
    (assert (find-shader name)))
  (let ((draw-mode (getf options :draw-mode)))
    (when draw-mode
      (check-type draw-mode draw-mode)))
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
           (loop :for shader :being :each :hash-value :of *vertices*
                 :when (slot-boundp shader 'vertex-array)
                   :do (destruct shader))
           (loop :for framebuffer :being :each :hash-value :of *framebuffers*
                 :do (destruct framebuffer))
           (loop :for texture :being :each :hash-value :of *textures*
                 :when (texture-id texture)
                   :do (destruct texture))
           (loop :for name :being :each :hash-key :of *shaders* :using
                      (:hash-value shader)
                 :when (program-id shader)
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
     (format output
             "Missing texture named ~S. ~:@_~? ~:@_To see all known textures, evaluate ~S."
             (cell-error-name this)
             "Did you mean ~#[~;~S~;~S or ~S~:;~S, ~S or ~S~] ?"
             (fuzzy-match:fuzzy-match (symbol-name (cell-error-name this))
                                      (list-all-textures))
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
            :else :if (locally
                       #+sbcl ; Due to second argument is not a (simple-array
                              ; character (*)).
                       (declare
                        (sb-ext:muffle-conditions sb-ext:compiler-note))
                       (equal "sampler2D" (uniform-type uniform)))
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

;;;; WITH-CLEAR

(defmacro with-clear
          (&whole whole
           (var-win (&rest buf*) &key (color ''(0.0 0.0 0.0 1.0)) (fps 60))
           &body body)
  (declare ((mod #.most-positive-fixnum) fps)
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (check-bnf:check-bnf (:whole whole)
    ((var-win symbol))
    ((buf* buffer-bit))
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

(defstruct framebuffer
  (id nil :type (or null (unsigned-byte 32)))
  texture
  render-buffer
  (format :rgb :type base-internal-format :read-only t)
  (pixel-type :unsigned-byte :type pixel-type :read-only t)
  (options nil :type list :read-only t)
  (attachment :color-attachment0 :type attachment :read-only t)
  (renderbuffer-initializer #'default-renderbuffer-initializer
                            :type function
                            :read-only t)
  (width (alexandria:required-argument :width)
         :type (mod #.most-positive-fixnum)
         :read-only t)
  (height (alexandria:required-argument :height)
          :type (mod #.most-positive-fixnum)
          :read-only t))

(defvar *framebuffers* (make-hash-table :test #'eq))

(defun list-all-framebuffers () (alexandria:hash-table-keys *framebuffers*))

(define-condition missing-framebuffer (fude-gl-error cell-error)
  ()
  (:report
   (lambda (this output)
     (format output
             "Missing framebuffer named ~S. ~:@_~? ~:@_To see all known framebuffer, evaluate ~S."
             (cell-error-name this)
             "Did you mean ~#[~;~S~;~S or ~S~:;~S, ~S or ~S~] ?"
             (fuzzy-match:fuzzy-match (symbol-name (cell-error-name this))
                                      (list-all-framebuffers))
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
                                           #+sbcl ; due to our responsibility.
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
     (setf (gethash ',name *framebuffers*) (make-framebuffer ,@params))))

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
  (3d-vectors::%vsetf (camera-position camera) x y z)
  camera)

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

(defparameter *images* (make-hash-table :test #'equal))

(defun list-all-images () (alexandria:hash-table-keys *images*))

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

(defun load-image (filename)
  (efiletype-case (truename filename)
    ((png) (opticl:vertical-flip-image (opticl:read-png-file filename)))
    ((jpg jpeg) (opticl:vertical-flip-image (opticl:read-jpeg-file filename)))))

;; DEFIMAGE

(defmacro defimage (name pathname)
  `(progn (setf (gethash ',name *images*) (load-image ,pathname)) ',name))