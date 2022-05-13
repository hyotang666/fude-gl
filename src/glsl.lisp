(in-package :fude-gl)

(defstruct environment
  (variable nil :type list :read-only t)
  (function nil :type list)
  (next nil :type (or null environment) :read-only t))

(defmethod print-object ((o environment) output)
  (print-unreadable-object (o output :type t :identity t)))

(defvar *environment* nil)

(deftype glsl-type ()
  '(member :bool :int :uint :float :vec2 :vec3 :vec4 :uvec3 :mat4 :|sampler2D|))

(defstruct variable-information
  (var (error "VAR is required.") :type symbol :read-only t)
  (name (error "NAME is required.") :type string :read-only t)
  (type (error "TYPE is required.")
        :type (member :attribute :io :uniform :varying :local :global :slot)
        :read-only t)
  (glsl-type nil :type (or list glsl-type) :read-only t)
  (ref? nil :type boolean))

(defun variable-information (symbol &optional env)
  (let* ((global?
          (cond ((uiop:string-prefix-p "gl_" symbol) (symbol-name symbol))
                ((uiop:string-prefix-p "GL-" symbol)
                 (let ((name
                        (format nil "gl_~A"
                                (change-case:pascal-case
                                  (subseq (symbol-name symbol) 3)))))
                   name))))
         (pred
          (if global?
              (lambda (info)
                (and (null (variable-information-var info))
                     (equal global? (variable-information-name info))))
              (lambda (info) (eq symbol (variable-information-var info))))))
    (labels ((rec (env)
               (unless (null env)
                 (or (find-if pred (environment-variable env))
                     (rec (environment-next env))))))
      (rec env))))

(defun slot-truename (spec)
  (etypecase spec (symbol spec) ((cons symbol (cons symbol null)) (cadr spec))))

(defgeneric var-info (type source &key)
  (:method (type (source list) &key)
    (mapcar
      (lambda (spec)
        (make-variable-information :var (car spec)
                                   :name (symbol-camel-case (car spec))
                                   :type type
                                   :glsl-type (cadr spec)))
      source))
  (:method ((type (eql :slot)) (source list) &key structure)
    (mapcar
      (lambda (spec)
        (make-variable-information :var (alexandria:ensure-car spec)
                                   :name (format nil "~A.~A"
                                                 (symbol-camel-case structure)
                                                 (symbol-camel-case
                                                   (slot-truename spec)))
                                   :type type))
      source))
  (:method ((type (eql :attribute)) (source list) &key)
    (mapcar
      (lambda (spec)
        (make-variable-information :var spec
                                   :name (symbol-camel-case spec)
                                   :type type))
      source))
  (:method ((type (eql :global)) (source list) &key)
    (mapcar
      (lambda (spec)
        (make-variable-information :var nil
                                   :name (car spec)
                                   :type type
                                   :glsl-type (cadr spec)))
      source)))

(defun list-all-known-vars ()
  (labels ((rec (env acc)
             (if (null env)
                 acc
                 (rec (environment-next env)
                      (progn
                       (loop :for info :in (environment-variable env)
                             :do (push (variable-information-name info) acc))
                       acc)))))
    (rec *environment* nil)))

(define-condition unused-variable (style-warning)
  ((name :initarg :name :reader unused-var))
  (:report
   (lambda (this output)
     (format output "Variable ~S is not used." (unused-var this)))))

(defun check-ref (vars)
  (dolist (var vars)
    (labels ((rec (env)
               (unless (null env)
                 (let ((info
                        (find var (environment-variable env)
                              :test #'eq
                              :key #'variable-information-var)))
                   (if info
                       (when (not (variable-information-ref? info))
                         (let ((*print-pprint-dispatch*
                                (copy-pprint-dispatch nil)))
                           (warn 'unused-variable :name var)))
                       (rec (environment-next env)))))))
      (rec *environment*))))

(defun function-information (symbol &optional env)
  (let ((name (symbol-camel-case symbol)))
    (labels ((rec (env)
               (unless (null env)
                 (or (find name (environment-function env) :test #'equal)
                     (rec (environment-next env))))))
      (rec env))))

(defun list-all-known-functions ()
  (labels ((rec (env acc)
             (if (null env)
                 acc
                 (rec (environment-next env)
                      (progn
                       (loop :for name :in (environment-function env)
                             :do (push name acc))
                       acc)))))
    (rec *environment* nil)))

(defun argument-environment (env &key variable function)
  (make-environment :next env :variable variable :function function))

(defun symbol-camel-case (s) (change-case:camel-case (symbol-name s)))

(defvar *var-check-p* nil)

(define-condition fude-gl-error (error) ())

(define-condition shader-error (fude-gl-error program-error) ())

(define-condition unknown-variable (shader-error cell-error)
  ((known-vars :initarg :known-vars :reader known-vars))
  (:report
   (lambda (this output)
     (pprint-logical-block (output nil)
       (apply #'format output
              "Unknown variable. ~S ~:@_Did you mean ~#[~;~S~;~S or ~S~:;~S, ~S or ~S~] ?"
              (cell-error-name this)
              (fuzzy-match:fuzzy-match
                (symbol-camel-case (cell-error-name this))
                (known-vars this)))))))

(defun glsl-symbol (stream exp &optional (errorp *var-check-p*) not-ref-p)
  ;; Ideally, we want to use REFERED-P rather than NOT-REF-P.
  ;; But, unfortunately, it is hard because this function is designed to
  ;; be used as a format function that specifies T when at-sign is specified.
  ;; (i.e. the default should be NIL.)
  ;; Of course, we can use (refered-p t suppliedp) with additional conditional branching.
  ;; Anyway, we choose to take a disgusting name rather than
  ;; a disgusting conditional branching code.
  "Print a symbol EXP to STREAM as GLSL code.
If EXP is unknown for compiler and ERRORP is true, condition UNKNOWN-VARIABLE is signaled
otherwise do nothing. The default is NIL. You can specify this by colon in format control.
If EXP is known for compiler and NOT-REF-P is ture, compiler memos it is refered
otherwise compiler do nothing. The default it NIL. You can specify this by at-sign in format control."
  (let ((info (variable-information exp *environment*)))
    (cond
      (info
       (unless not-ref-p ; means refered-p
         (unless (eq :global (variable-information-type info))
           (setf (variable-information-ref? info) t)))
       (write-string (variable-information-name info) stream))
      (errorp
       (error 'unknown-variable :name exp :known-vars (list-all-known-vars)))
      ((find-if #'lower-case-p (symbol-name exp))
       (write-string (symbol-name exp) stream))
      (t (write-string (symbol-camel-case exp) stream)))))

(defun glsl-setf (stream exp)
  (setf stream (or stream *standard-output*))
  (let ((*var-check-p* t))
    (funcall (formatter "~{~:/fude-gl::glsl-symbol/ = ~W;~:@_~}") stream
             (cdr exp))))

(define-condition unknown-glsl-function (fude-gl-error cell-error)
  ()
  (:report
   (lambda (this output)
     (format output
             "Unknown glsl function named ~S. ~:@_~? ~:@_To see all supported glsl functions, evaluate ~S."
             (cell-error-name this)
             "Did you mean ~#[~;~S~;~S or ~S~:;~S, ~S or ~S~] ?"
             (fuzzy-match:fuzzy-match
               (symbol-camel-case (cell-error-name this))
               (list-all-known-functions))
             '(list-all-known-functions)))))

(defun glsl-funcall (stream exp)
  (setf stream (or stream *standard-output*))
  (assert (function-information (car exp) *environment*) ()
    'unknown-glsl-function :name (car exp))
  (funcall (formatter "~/fude-gl:glsl-symbol/~:<~@{~W~^, ~@_~}~:>") stream
           (car exp) (cdr exp)))

(defun glsl-operator (stream exp)
  (setf stream (or stream *standard-output*))
  (let ((op (car exp)))
    (case (length (cdr exp))
      (0 (error "No argument ~S" exp))
      (1 (format stream "~A~A" (symbol-name op) (cadr exp)))
      (2
       (let ((*var-check-p* t))
         (format stream "(~W ~A ~W)" (second exp) (symbol-name op)
                 (third exp))))
      (otherwise
       (loop :for (form . rest) :on (cdr exp)
             :initially (write-char #\( stream)
             :do (let ((*var-check-p* t))
                   (write form :stream stream))
                 (when rest
                   (format stream " ~A " (symbol-name op)))
             :finally (write-char #\) stream))))))

(defun glsl-aref (stream exp)
  (setf stream (or stream *standard-output*))
  (let ((*var-check-p* t))
    (funcall (formatter "~:/fude-gl:glsl-symbol/~:<[~;~@{~W~^, ~@_~}~;]~:>")
             stream (cadr exp) (cddr exp))))

(defun glsl-let (stream exp)
  (setf stream (or stream *standard-output*))
  (labels ((rec (binds)
             (if (endp binds)
                 (progn
                  (funcall
                    (formatter
                     "~<~@{~W~^ ~:@/fude-gl:glsl-symbol/~^ = ~W;~:@_~}~:>~{~W~^ ~_~}")
                    stream
                    (loop :for (name type init) :in (cadr exp)
                          :do (check-type type glsl-type)
                          :collect type
                          :collect name
                          :collect init)
                    (cddr exp))
                  (when (every #'listp (cddr exp))
                    (check-ref (mapcar #'car (cadr exp)))))
                 (let ((*environment*
                        (argument-environment *environment*
                                              :variable (var-info :local (list
                                                                           (car
                                                                             binds))))))
                   (rec (cdr binds))))))
    (rec (cadr exp))))

(defun glsl-swizzling (stream exp)
  (setf stream (or stream *standard-output*))
  (let ((*var-check-p* t))
    (format stream "~W.~/fude-gl:glsl-symbol/" (cadr exp) (car exp))))

(defun glsl-return (stream exp)
  (setf stream (or stream *standard-output*))
  (format stream "~{~W~^ ~};" exp))

(defun parse-slot-spec (spec)
  (etypecase spec
    (symbol (values spec spec))
    ((cons symbol (cons symbol null)) (values-list spec))))

(define-condition unknown-slot (fude-gl-error cell-error)
  ((type :initarg :type :reader slot-type)
   (known-vars :initarg :known-vars :reader known-vars))
  (:report
   (lambda (this output)
     (format output "Unknown slot named ~S for type ~S. ~:@_~?"
             (cell-error-name this) (slot-type this)
             "Did you mean ~#[~;~S~;~S or ~S~:;~S, ~S or ~S~] ?"
             (fuzzy-match:fuzzy-match (symbol-name (cell-error-name this))
                                      (known-vars this))))))

(defun glsl-with-slots (stream exp)
  (setf stream (or stream *standard-output*))
  (destructuring-bind
      (slots type &body body)
      (cdr exp)
    (let ((info (variable-information type *environment*)))
      ;; TYPE existence checking.
      (assert info ()
        'unknown-variable :name type
                          :known-vars (list-all-known-vars))
      ;; SLOTS existence checking.
      (dolist (slot slots)
        (assert (assoc (slot-truename slot)
                       (variable-information-glsl-type info))
          ()
          'unknown-slot :name (slot-truename slot)
                        :type type
                        :known-vars (mapcar #'car
                                            (variable-information-glsl-type
                                              info))))
      (let ((*environment*
             (argument-environment *environment*
                                   :variable (var-info :slot slots
                                                       :structure type))))
        ;; The body.
        (dolist (exp body) (write exp :stream stream))))))

(defun glsl-if (stream exp)
  (setf stream (or stream *standard-output*))
  (destructuring-bind
      (pred then &optional else)
      (cdr exp)
    (format stream "~W ? ~W : ~W" pred then else)))

(defun glsl-define-symbol-macro (stream exp)
  (setf stream (or stream *standard-output*))
  (funcall (formatter "#define ~{~A~^ ~}~%") stream (cdr exp)))

(defvar *declaims* (make-hash-table :test #'eq))

(defun glsl-declaim (stream exp)
  (declare (ignore stream))
  (loop :for (key . param) :in (cdr exp)
        :when (find key '(type ftype))
          :do (dolist (name (cdr param))
                (setf (gethash name *declaims*) (car param)))
        :else
          :do (warn "Ignore decalim of ~S" (cons key param))))

(defun glsl-defconstant (stream exp)
  (setf stream (or stream *standard-output*))
  (unless (gethash (second exp) *declaims*)
    (error "CONSTANT ~S needs type DECLAIMed." (second exp)))
  (funcall (formatter "const ~A ~A = ~A;~%") stream
           (symbol-camel-case (gethash (second exp) *declaims*))
           (symbol-camel-case (second exp)) (third exp)))

(defun glsl-defun (stream exp)
  (setf stream (or stream *standard-output*))
  (let* ((ftype (gethash (second exp) *declaims*))
         (*environment*
          (progn
           (push (symbol-camel-case (second exp))
                 (environment-function *environment*))
           (argument-environment *environment*
                                 :variable (var-info :local (mapcar #'list
                                                                    (third exp)
                                                                    (cadr
                                                                      ftype)))))))
    (unless ftype
      (error "DEFUN ~S needs ftype DECLAIMed." (second exp)))
    (destructuring-bind
        (arg-types return)
        (cdr ftype)
      (funcall
        (formatter
         #.(apply #'concatenate 'string
                  (alexandria:flatten
                    (list "~(~A~)~^ ~@_" ; return type
                          "~A~^ ~@_" ; function name.
                          (list "~:<" ; logical block for args.
                                "~@{~(~A~)~^ ~A~^, ~}" ; argbody.
                                "~:>~^ ~%")
                          "~:<{~;~3I~:@_" ; function body.
                          "~@{~A~^ ~_~}~%" "~;}~:>~%"))))
        stream
        (if (equal '(values) return)
            :void
            return)
        (second exp) (mapcan #'list arg-types (third exp)) (cdddr exp)))
    (when (every #'listp (cdddr exp))
      (check-ref (third exp)))))

(defun glsl-dispatch ()
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil)))
    (set-pprint-dispatch 'symbol 'glsl-symbol)
    (set-pprint-dispatch '(cons (member setf)) 'glsl-setf)
    (set-pprint-dispatch '(cons (member aref)) 'glsl-aref)
    (set-pprint-dispatch '(cons (member * + / - <)) 'glsl-operator)
    (set-pprint-dispatch '(cons (member let)) 'glsl-let)
    (set-pprint-dispatch '(cons symbol) 'glsl-funcall -1)
    (set-pprint-dispatch '(cons (member r rgb z w xy xyz)) 'glsl-swizzling)
    (set-pprint-dispatch '(cons (member return)) 'glsl-return)
    (set-pprint-dispatch '(cons (member with-slots)) 'glsl-with-slots)
    (set-pprint-dispatch '(cons (member if)) 'glsl-if)
    (set-pprint-dispatch '(cons (member define-symbol-macro))
                         'glsl-define-symbol-macro)
    (set-pprint-dispatch '(cons (member declaim)) 'glsl-declaim)
    (set-pprint-dispatch '(cons (member defconstant)) 'glsl-defconstant)
    (set-pprint-dispatch '(cons (member defun)) 'glsl-defun)
    *print-pprint-dispatch*))

(defun print-glsl (exp &optional stream)
  (handler-case
      (let ((*print-pretty* t) (*print-pprint-dispatch* (glsl-dispatch)))
        (pprint exp stream))
    (error (e)
      (error e))))

(defun pprint-glsl (stream exp &rest noise)
  (declare (ignore noise))
  (print-glsl exp stream))

(setq *environment*
        (make-environment :next nil
                          :variable (var-info :global '(("gl_ClipDistance"
                                                         :float)
                                                        ("gl_CullDistance"
                                                         :float)
                                                        ("gl_FragCoord" :vec4)
                                                        ("gl_FragDepth" :float)
                                                        ("gl_FrontFacing"
                                                         :bool)
                                                        ("gl_GlobalInvocationID"
                                                         :uvec3)
                                                        ("gl_HelperInvocation"
                                                         :bool)
                                                        ("gl_InstanceID" :int)
                                                        ("gl_InvocationID"
                                                         :int)
                                                        ("gl_Layer" :int)
                                                        ("gl_LocalInvocationID"
                                                         :uvec3)
                                                        ("gl_LocalInvocationIndex"
                                                         :uint)
                                                        ("gl_NumSamples" :bool)
                                                        ("gl_NumWorkGroups"
                                                         :uvec3)
                                                        ("gl_PatchVerticesIn"
                                                         :int)
                                                        ("gl_PointCoord" :vec2)
                                                        ("gl_PointSize" :float)
                                                        ("gl_Position" :vec4)
                                                        ("gl_PrimitiveID" :int)
                                                        ("gl_PrimitiveIDIn"
                                                         :int)
                                                        ("gl_SampleID" :int)
                                                        ("gl_SampleMask" :int)
                                                        ("gl_SampleMaskIn"
                                                         :int)
                                                        ("gl_SamplePosition"
                                                         :vec2)
                                                        ("gl_TessCoord" :vec3)
                                                        ("gl_TessLevelInner"
                                                         :float)
                                                        ("gl_TessLevelOuter"
                                                         :float)
                                                        ("gl_VertexID" :int)
                                                        ("gl_ViewportIndex"
                                                         :int)
                                                        ("gl_WorkGroupID"
                                                         :uvec3)
                                                        ("gl_WorkGroupSize"
                                                         :uvec3)))
                          :function '(;;;; Built in functions.
                                      ;;;; https://www.khronos.org/registry/OpenGL-Refpages/gl4/index.php
                                      "abs" "acos" "acosh" "all" "any" "asin"
                                      "asinh" "atan" "atanh" "atomicAdd"
                                      "atomicAnd" "atomicCompSwap"
                                      "atomicCounter" "atomicCounterDecrement"
                                      "atomicCounterIncrement" "atomicExchange"
                                      "atomicMax" "atomicMin" "atomicOr"
                                      "atomicXor" "barrier" "bitCount"
                                      "bitfieldExtract" "bitfieldInsert"
                                      "bitfieldReverse" "ceil" "clamp" "cos"
                                      "cosh" "cross" "degrees" "determinant"
                                      "dFdx" "dFdxCoarse" "dFdxFine" "dFdy"
                                      "dFdyCoarse" "dFdyFine" "distance" "dot"
                                      "EmitStreamVertex" "EmitVertex"
                                      "EndPrimitive" "EndStreamPrimitive"
                                      "equal" "exp" "exp2" "faceforward"
                                      "findLSB" "findMSB" "floatBitsToInt"
                                      "floatBitsToUint" "floor" "fma" "fract"
                                      "frexp" "fwidth" "fwidthCoarse"
                                      "fwidthFine" "greaterThan"
                                      "greaterThanEqual" "groupMemoryBarrier"
                                      "imageAtomicAdd" "imageAtomicAnd"
                                      "imageAtomicCompSwap"
                                      "imageAtomicExchange" "imageAtomicMax"
                                      "imageAtomicMin" "imageAtomicOr"
                                      "imageAtomicXor" "imageLoad"
                                      "imageSamples" "imageSize" "imageStore"
                                      "imulExtended" "intBitsToFloat"
                                      "interpolateAtCentroid"
                                      "interpolateAtOffset"
                                      "interpolateAtSample" "inverse"
                                      "inversesqrt" "isinf" "isnan" "ldexp"
                                      "length" "lessThan" "lessThanEqual" "log"
                                      "log2" "matrixCompMult" "max"
                                      "memoryBarrier"
                                      "memoryBarrierAtomicCounter"
                                      "memoryBarrierBuffer"
                                      "memoryBarrierImage"
                                      "memoryBarrierShared" "min" "mix" "mod"
                                      "modf" "noise" "noise1" "noise2" "noise3"
                                      "noise4" "normalize" "not" "notEqual"
                                      "outerProduct" "packDouble2x32"
                                      "packHalf2x16" "packSnorm2x16"
                                      "packSnorm4x8" "packUnorm"
                                      "packUnorm2x16" "packUnorm4x8" "pow"
                                      "radians" "reflect" "refract" "round"
                                      "roundEven" "sign" "sin" "sinh"
                                      "smoothstep" "sqrt" "step" "tan" "tanh"
                                      "texelFetch" "texelFetchOffset" "texture"
                                      "textureGather" "textureGatherOffset"
                                      "textureGatherOffsets" "textureGrad"
                                      "textureGradOffset" "textureLod"
                                      "textureLodOffset" "textureOffset"
                                      "textureProj" "textureProjGrad"
                                      "textureProjGradOffset" "textureProjLod"
                                      "textureProjLodOffset"
                                      "textureProjOffset" "textureQueryLevels"
                                      "textureQueryLod" "textureSamples"
                                      "textureSize" "transpose" "trunc"
                                      "uaddCarry" "uintBitsToFloat"
                                      "umulExtended" "unpackDouble2x32"
                                      "unpackHalf2x16" "unpackSnorm2x16"
                                      "unpackSnorm4x8" "unpackUnorm"
                                      "unpackUnorm2x16" "unpackUnorm4x8"
                                      "usubBorrow"
                                      ;;;; Type constructors.
                                      ;;;; https://www.khronos.org/opengl/wiki/Data_Type_(GLSL)#Basic_types
                                      ;; > Vectors
                                      ;; > Each of the scalar types, including booleans, have 2, 3, and 4-component vector equivalents. The n digit below can be 2, 3, or 4:
                                      "bvec2" "bvec3" "bvec4" "ivec2" "ivec3"
                                      "ivec4" "uvec2" "uvec3" "uvec4" "vec2"
                                      "vec3" "vec4" "dvec2" "dvec3" "dvec4"
                                      ;; > Matrices
                                      ;; > In addition to vectors, there are also matrix types. All matrix types are floating-point, either single-precision or double-precision. Matrix types are as follows, where n and m can be the numbers 2, 3, or 4:
                                      "mat2x2" "mat2x3" "mat2x4" "mat3x2"
                                      "mat3x3" "mat3x4" "mat4x2" "mat4x3"
                                      "mat4x4" "mat2" "mat3" "mat4"
                                      ;;;; Operator
                                      "*" "/" "+" "-" "<" ">" "<=" ">=" "=="
                                      "&&" "^^" "||" "%" "<<" ">>" "&" "^"
                                      "|")))