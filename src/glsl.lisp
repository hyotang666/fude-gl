(in-package :fude-gl)

(defvar *glsl-functions*
  (uiop:list-to-hash-set
    '(;;;; Built in functions.
      ;;;; https://www.khronos.org/registry/OpenGL-Refpages/gl4/index.php
      "abs" "acos" "acosh" "all" "any" "asin" "asinh" "atan" "atanh"
      "atomicAdd" "atomicAnd" "atomicCompSwap" "atomicCounter"
      "atomicCounterDecrement" "atomicCounterIncrement" "atomicExchange"
      "atomicMax" "atomicMin" "atomicOr" "atomicXor" "barrier" "bitCount"
      "bitfieldExtract" "bitfieldInsert" "bitfieldReverse" "ceil" "clamp" "cos"
      "cosh" "cross" "degrees" "determinant" "dFdx" "dFdxCoarse" "dFdxFine"
      "dFdy" "dFdyCoarse" "dFdyFine" "distance" "dot" "EmitStreamVertex"
      "EmitVertex" "EndPrimitive" "EndStreamPrimitive" "equal" "exp" "exp2"
      "faceforward" "findLSB" "findMSB" "floatBitsToInt" "floatBitsToUint"
      "floor" "fma" "fract" "frexp" "fwidth" "fwidthCoarse" "fwidthFine"
      "gl_ClipDistance" "gl_CullDistance" "gl_FragCoord" "gl_FragDepth"
      "gl_FrontFacing" "gl_GlobalInvocationID" "gl_HelperInvocation"
      "gl_InstanceID" "gl_InvocationID" "gl_Layer" "gl_LocalInvocationID"
      "gl_LocalInvocationIndex" "gl_NumSamples" "gl_NumWorkGroups"
      "gl_PatchVerticesIn" "gl_PointCoord" "gl_PointSize" "gl_Position"
      "gl_PrimitiveID" "gl_PrimitiveIDIn" "gl_SampleID" "gl_SampleMask"
      "gl_SampleMaskIn" "gl_SamplePosition" "gl_TessCoord" "gl_TessLevelInner"
      "gl_TessLevelOuter" "gl_VertexID" "gl_ViewportIndex" "gl_WorkGroupID"
      "gl_WorkGroupSize" "greaterThan" "greaterThanEqual" "groupMemoryBarrier"
      "imageAtomicAdd" "imageAtomicAnd" "imageAtomicCompSwap"
      "imageAtomicExchange" "imageAtomicMax" "imageAtomicMin" "imageAtomicOr"
      "imageAtomicXor" "imageLoad" "imageSamples" "imageSize" "imageStore"
      "imulExtended" "intBitsToFloat" "interpolateAtCentroid"
      "interpolateAtOffset" "interpolateAtSample" "inverse" "inversesqrt"
      "isinf" "isnan" "ldexp" "length" "lessThan" "lessThanEqual" "log" "log2"
      "matrixCompMult" "max" "memoryBarrier" "memoryBarrierAtomicCounter"
      "memoryBarrierBuffer" "memoryBarrierImage" "memoryBarrierShared" "min"
      "mix" "mod" "modf" "noise" "noise1" "noise2" "noise3" "noise4"
      "normalize" "not" "notEqual" "outerProduct" "packDouble2x32"
      "packHalf2x16" "packSnorm2x16" "packSnorm4x8" "packUnorm" "packUnorm2x16"
      "packUnorm4x8" "pow" "radians" "reflect" "refract" "round" "roundEven"
      "sign" "sin" "sinh" "smoothstep" "sqrt" "step" "tan" "tanh" "texelFetch"
      "texelFetchOffset" "texture" "textureGather" "textureGatherOffset"
      "textureGatherOffsets" "textureGrad" "textureGradOffset" "textureLod"
      "textureLodOffset" "textureOffset" "textureProj" "textureProjGrad"
      "textureProjGradOffset" "textureProjLod" "textureProjLodOffset"
      "textureProjOffset" "textureQueryLevels" "textureQueryLod"
      "textureSamples" "textureSize" "transpose" "trunc" "uaddCarry"
      "uintBitsToFloat" "umulExtended" "unpackDouble2x32" "unpackHalf2x16"
      "unpackSnorm2x16" "unpackSnorm4x8" "unpackUnorm" "unpackUnorm2x16"
      "unpackUnorm4x8" "usubBorrow"
      ;;;; Type constructors.
      ;;;; https://www.khronos.org/opengl/wiki/Data_Type_(GLSL)#Basic_types
      ;; > Vectors
      ;; > Each of the scalar types, including booleans, have 2, 3, and 4-component vector equivalents. The n digit below can be 2, 3, or 4:
      "bvec2" "bvec3" "bvec4" "ivec2" "ivec3" "ivec4" "uvec2" "uvec3" "uvec4"
      "vec2" "vec3" "vec4" "dvec2" "dvec3" "dvec4"
      ;; > Matrices
      ;; > In addition to vectors, there are also matrix types. All matrix types are floating-point, either single-precision or double-precision. Matrix types are as follows, where n and m can be the numbers 2, 3, or 4:
      "mat2x2" "mat2x3" "mat2x4" "mat3x2" "mat3x3" "mat3x4" "mat4x2" "mat4x3"
      "mat4x4" "mat2" "mat3" "mat4"
      ;;;; Operator
      "*" "/" "+" "-" "<" ">" "<=" ">=" "==" "&&" "^^" "||" "%" "<<" ">>" "&"
      "^" "|")))

(defvar *shader-vars*)

(defun symbol-camel-case (s) (change-case:camel-case (symbol-name s)))

(defvar *alias* nil)

(deftype glsl-type () '(member :float :vec2 :vec3 :vec4 :mat4 :|sampler2D|))

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
              (fuzzy-match:fuzzy-match (symbol-name (cell-error-name this))
                                       (known-vars this)))))))

(defun glsl-symbol (stream exp &optional (colonp *var-check-p*) atp)
  (declare (ignore atp))
  (flet ((check-builtin-var-existence (name exp)
           (when colonp
             (assert (gethash name *glsl-functions*) ()
               'unknown-variable :name exp
                                 :known-vars (loop :for key :being :each
                                                        :hash-key :of
                                                        *glsl-functions*
                                                   :when (uiop:string-prefix-p
                                                           "gl_" key)
                                                     :collect key)))))
    (let ((alias (assoc exp *alias*)))
      (cond (alias (glsl-symbol stream (cdr alias)))
            ((uiop:string-prefix-p "gl_" exp)
             (check-builtin-var-existence (symbol-name exp) exp)
             (write-string (symbol-name exp) stream))
            ((uiop:string-prefix-p "GL-" exp)
             (let ((name
                    (format nil "gl_~A"
                            (change-case:pascal-case
                              (subseq (symbol-name exp) 3)))))
               (check-builtin-var-existence name exp)
               (write-string name stream)))
            ((progn
              (when colonp
                (unless (gethash exp *shader-vars*)
                  (error 'unknown-variable
                         :name exp
                         :known-vars (alexandria:hash-table-keys
                                       *shader-vars*))))
              (find-if #'lower-case-p (symbol-name exp)))
             (write-string (symbol-name exp) stream))
            (t
             (write-string (change-case:camel-case (symbol-name exp))
                           stream))))))

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
             (fuzzy-match:fuzzy-match (symbol-name (cell-error-name this))
                                      (alexandria:hash-table-keys
                                        *glsl-functions*))
             '(alexandria:hash-table-keys *glsl-functions*)))))

(defun glsl-funcall (stream exp)
  (setf stream (or stream *standard-output*))
  (assert (gethash (symbol-camel-case (car exp)) *glsl-functions*) ()
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
  (funcall (formatter "~<~@{~W~^ ~W~^ = ~W;~:@_~}~:>~{~W~^ ~_~}") stream
           (loop :for (name type init) :in (cadr exp)
                 :do (check-type type glsl-type)
                     (setf (gethash name *shader-vars*) t)
                 :collect type
                 :collect name
                 :collect init)
           (cddr exp)))

(defun glsl-swizzling (stream exp)
  (setf stream (or stream *standard-output*))
  (let ((*var-check-p* t))
    (format stream "~W.~/fude-gl:glsl-symbol/" (cadr exp) (car exp))))

(defun glsl-return (stream exp)
  (setf stream (or stream *standard-output*))
  (format stream "~{~W~^ ~};" exp))

(defun glsl-with-slots (stream exp)
  (setf stream (or stream *standard-output*))
  (destructuring-bind
      (slots type &body body)
      (cdr exp)
    (let ((*alias*
           (pairlis (mapcar #'alexandria:ensure-car slots)
                    (loop :for slot :in slots
                          :collect (intern
                                     (format nil "~A.~A" type
                                             (if (symbolp slot)
                                                 slot
                                                 (cadr slot)))))
                    *alias*)))
      (dolist (exp body) (write exp :stream stream)))))

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
  (let ((ftype (gethash (second exp) *declaims*))
        (*shader-vars* (alexandria:copy-hash-table *shader-vars*)))
    (dolist (var (third exp)) (setf (gethash var *shader-vars*) t))
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
        (second exp) (mapcan #'list arg-types (third exp)) (cdddr exp)))))

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