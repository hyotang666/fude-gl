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

(deftype layout-input-primitive ()
  '(member :points :lines :lines-adjacency :triangles :triangles-adjacency))

(deftype layout-output-primitive ()
  '(member :points :line-strip :triangle-strip))

;;;; PROTECT

(defvar *cleaners* nil)

(defmacro protect (<cleanup>)
  `(handler-case ,<cleanup>
     (error (c)
       (push (lambda () ,<cleanup>) *cleaners*)
       (cerror "Go to next cleanup form."
               "~A ~:@_FYI: Current cleanup form is pushed to ~S as nullary closure."
               c '*cleaners*))))

;;;; VERTEX-ATTRIBUTE-CLASSES

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; DEFINE-VERTEX-ATTRIBUTE below needs this too.
  (defvar *vertex-attributes* (make-hash-table))
  (defun vertex-attribute-p (thing)
    (and (symbolp thing) (values (gethash thing *vertex-attributes*)))))

(defun list-all-attributes () (alexandria:hash-table-keys *vertex-attributes*))

;;;; METACLASS

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

(define-vertex-attribute u ())

(define-vertex-attribute rgb ())

(define-vertex-attribute offset (x y) (:instances t))

(define-vertex-attribute frag (bool) (:instances t))

(define-vertex-attribute a () (:instances t))

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

;;;; UNIFORM-BUFFER-OBJECT

(defvar *uniform-buffer-objects* (make-hash-table :test #'eq))

(defclass uniform-buffer-object-class (standard-class)
  ((related-shaders :type list :initform nil :accessor related-shaders)
   (buffer :type (or null buffer) :initform nil :accessor buffer)
   (layout :type keyword :initform :std140 :initarg :layout :reader layout)))

(defmethod c2mop:validate-superclass
           ((s uniform-buffer-object-class) (c standard-class))
  t)

(defclass uniform-buffer-object-slot-mixin ()
  ((glsl-type :type glsl-type :initarg :glsl-type :reader glsl-type))
  (:documentation
   "Meta informations for the slot of the uniform buffer object."))

(defclass uniform-buffer-object-direct-slot-definition
    (c2mop:standard-direct-slot-definition uniform-buffer-object-slot-mixin)
  ())

(defmethod c2mop:direct-slot-definition-class
           ((s uniform-buffer-object-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'uniform-buffer-object-direct-slot-definition))

(defclass uniform-buffer-object-effective-slot
    (c2mop:standard-effective-slot-definition uniform-buffer-object-slot-mixin)
  ())

(defmethod c2mop:effective-slot-definition-class
           ((s uniform-buffer-object-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'uniform-buffer-object-effective-slot))

(defmethod c2mop:compute-effective-slot-definition
           ((o uniform-buffer-object-class) slot-name direct-slot-definitions)
  (let ((effective-slot (call-next-method)))
    (setf (slot-value effective-slot 'glsl-type)
            (glsl-type
              (or (find (the symbol slot-name)
                        (the list direct-slot-definitions)
                        :key #'c2mop:slot-definition-name)
                  (error "Missing slot named ~S in ~S." slot-name
                         direct-slot-definitions))))
    effective-slot))

(defclass uniform-buffer-object () ())

(defmethod initialize-instance :after ((o uniform-buffer-object) &key layout)
  (let ((c (class-of o)))
    (setf (slot-value c 'layout) layout
          (slot-value c 'related-shaders) nil
          (slot-value c 'buffer)
            (make-buffer :name (class-name (class-of o))
                         :original (make-array 0 :element-type 'single-float)
                         :target :uniform-buffer))))

(defmacro defubo (name&options &body slot+)
  (destructuring-bind
      (name . options)
      (uiop:ensure-list name&options)
    `(eval-when (:compile-toplevel :execute)
       ;; When shader is inherited, check-bnf in defshader needs this eval-when.
       ;; Side-effect that sets related-shaders is done only in compile time so
       ;; :load-toplevel is never required.
       ;; MEMO: Should we make side-effect works in run-time?
       (defclass ,name (uniform-buffer-object)
         ,(mapcar
            (lambda (slot)
              `(,(car slot) :glsl-type ,(cadr slot) :initform nil))
            slot+)
         (:metaclass uniform-buffer-object-class)
         (:default-initargs :layout
          ,(or (cadr (assoc :layout options)) :std140)))
       (setf (gethash ',name *uniform-buffer-objects*)
               (make-instance ',name)))))

(set-pprint-dispatch '(cons (member defubo)) (pprint-dispatch '(defstruct)))

(defmethod construct ((o uniform-buffer-object))
  (let* ((class (class-of o)) (slots (c2mop:class-slots class)))
    ;; Initialize each slot.
    (dolist (slot slots)
      (unless (slot-value o (c2mop:slot-definition-name slot))
        (setf (slot-value o (c2mop:slot-definition-name slot))
                (gl:alloc-gl-array (foreign-type 'single-float :cffi t)
                                   (case (glsl-type slot)
                                     (:mat4 16)
                                     (otherwise "NIY ~S" (glsl-type slot)))))))
    (let ((uniform-name
           (change-case:pascal-case (symbol-name (class-name class)))))
      ;; Link each shader's uniform block to the uniform binding point.
      (dolist (shader (related-shaders class))
        (let ((shader-id (find-program shader :if-does-not-exist :create)))
          (%gl:uniform-block-binding shader-id
                                     (gl:get-uniform-block-index shader-id
                                                                 uniform-name)
                                     0))))
    (let ((total-size
           (reduce #'+ slots
                   :key (lambda (slot)
                          #+sbcl ; Not our responsebilities.
                          (declare
                           (sb-ext:muffle-conditions sb-ext:compiler-note))
                          (gl:gl-array-byte-size
                            (slot-value o
                                        (c2mop:slot-definition-name slot))))))
          (buffer (buffer class)))
      ;; Now actually create the buffer.
      (in-buffer (construct buffer))
      (locally
       #+sbcl ; Not our responsebilities.
       (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
       (%gl:buffer-data (buffer-target buffer) total-size (cffi:null-pointer)
                        (buffer-usage buffer)))
      (gl:bind-buffer :uniform-buffer 0)
      ;; Define the range of the buffer that links to a uniform binding point.
      (%gl:bind-buffer-range (buffer-target buffer) 0 (buffer-buffer buffer) 0
                             total-size)))
  o)

(defmethod destruct ((o uniform-buffer-object))
  (dolist (slot (c2mop:class-slots (class-of o)))
    (when (slot-value o (c2mop:slot-definition-name slot))
      (gl:free-gl-array (slot-value o (c2mop:slot-definition-name slot)))
      (setf (slot-value o (c2mop:slot-definition-name slot)) nil)))
  (with-slots (buffer)
      (class-of o)
    (when buffer
      (destruct buffer))))

(define-condition missing-uniform-buffer-object (fude-gl-error cell-error)
  ()
  (:report
   (lambda (this out)
     (format out "Missing uniform buffer object named ~S."
             (cell-error-name this)))))

(defun find-uniform-buffer-object (name &key (if-does-not-exist :error))
  (let ((ubo (gethash name *uniform-buffer-objects*)))
    (cond
      ((null ubo) ; Not defined yet.
       (error 'missing-uniform-buffer-object :name name))
      ((buffer-buffer (buffer (class-of ubo))) ; Defined and constructed.
       ubo)
      (t
       (case if-does-not-exist
         (:error
          (cerror "Return uniform buffer object without constructing."
                  "Uniform buffer object named ~S is not constructed in openGL yet."
                  name)
          ubo)
         (:create (construct ubo))
         ((nil) ubo))))))

(defun uniform-buffer-object-name-p (thing)
  (and (symbolp thing) (gethash thing *uniform-buffer-objects*)))

(defun list-all-uniform-buffer-objects ()
  (alexandria:hash-table-keys *uniform-buffer-objects*))

;;;; DEFSHADER

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

(defun uniform-keywordp (thing) (and (symbolp thing) (string= '&uniform thing)))

(defun varying-keywordp (thing) (and (symbolp thing) (string= '&varying thing)))

(deftype var () 'symbol)

(deftype type-spec () '(or keyword (satisfies glsl-structure-name-p)))

(deftype index () '(mod #.array-total-size-limit))

(deftype constant-definition ()
  '(cons (eql defconstant) (cons symbol (cons index null))))

(declaim
 (ftype (function (list) (values list list list &optional))
        split-shader-lambda-list))

(defun split-shader-lambda-list (lambda-list)
  (uiop:while-collecting (out uniform varying)
    (loop :with collector = #'out
          :with attribute = :io
          :for elt :in lambda-list
          :if (uniform-keywordp elt)
            :do (setf collector #'uniform
                      attribute :uniform)
          :else :if (varying-keywordp elt)
            :do (setf collector #'varying
                      attribute :varying)
          :else
            :do (funcall collector elt))))

(defun lisp-type<-glsl-type (glsl-type)
  (if (glsl-structure-name-p glsl-type)
      glsl-type
      (ecase glsl-type ; YAGNI style.
        (:vec2 '(or 3d-vectors:vec2 (vector single-float 2)))
        (:vec3 '(or 3d-vectors:vec3 (vector single-float 3)))
        (:mat4 '3d-matrices:mat4)
        (:float 'single-float)
        ((:|sampler2D| :sampler-cube) 'texture))))

(defparameter *converter* 'lisp-type<-glsl-type)

(defstruct uniform
  (name (error "NAME is required.") :type string :read-only t)
  (lisp-type (error "LISP-TYPE is required.") :type t :read-only t)
  (glsl-type (error "GLSL-TYPE is rqeuired.") :type symbol :read-only t))

(defmethod make-load-form ((o uniform) &optional env)
  (make-load-form-saving-slots o :environment env))

(defun <uniforms> (name shader*)
  ;; Shader NAME becomes VECTOR-CLASS.
  ;; MAKE-INSTANCE for VECTOR-CLASS makes a VECTOR instead of an object instance.
  ;; So we should use eql-specializer instead of class.
  `(defmethod uniforms ((type (eql ',name)))
     (list
       ,@(labels ((slot-reader-uniform (var slot)
                    (make-uniform :name (format nil "~A.~A"
                                                (symbol-camel-case var)
                                                (symbol-camel-case
                                                  (c2mop:slot-definition-name
                                                    slot)))
                                  :lisp-type (c2mop:slot-definition-type slot)
                                  :glsl-type (glsl-type slot)))
                  (convert (type)
                    (funcall (coerce *converter* 'function) type)))
           (uiop:while-collecting (acc)
             (loop :for (nil lambda-list . main) :in shader*
                   :if (typep main
                              '(cons (and symbol (satisfies find-shader)) null))
                     :do (mapc #'acc (uniforms (car main)))
                   :else
                     :do (dolist
                             (uniform-spec
                              (nth-value 1
                                         (split-shader-lambda-list
                                           lambda-list)))
                           (if (listp uniform-spec)
                               (destructuring-bind
                                   (var type . vector-size)
                                   uniform-spec
                                 (declare (ignore vector-size))
                                 (acc
                                   (make-uniform :name (symbol-camel-case var)
                                                 :lisp-type (convert type)
                                                 :glsl-type type))
                                 (when (glsl-structure-name-p type)
                                   (dolist
                                       (slot
                                        (c2mop:class-direct-slots
                                          (find-class type)))
                                     (acc (slot-reader-uniform var slot)))))
                               ;; Uniform buffer object.
                               (let ((ubo
                                      (class-of
                                        (find-uniform-buffer-object
                                          uniform-spec
                                          :if-does-not-exist nil))))
                                 (dolist
                                     (slot
                                      (c2mop:class-slots
                                        (c2mop:ensure-finalized ubo)))
                                   (acc
                                     (make-uniform :name (symbol-camel-case
                                                           (c2mop:slot-definition-name
                                                             slot))
                                                   :lisp-type 'uniform-buffer-object
                                                   :glsl-type (class-name
                                                                ubo)))))))))))))

(defun struct-defs (specs)
  (loop :for spec :in specs
        :for (nil type) := (and (listp spec) spec)
        :when (glsl-structure-name-p type)
          :collect type))

(defun constant-defs (specs)
  (loop :for spec :in specs
        :for (nil nil . vector-size) := (and (listp spec) spec)
        :when (and vector-size (typep (car vector-size) 'constant-definition))
          :collect (car vector-size)))

(defun glsl-declaration (out info &optional colonp atp)
  (pprint-logical-block (out nil)
    (let ((style
           (if colonp
               (if atp
                   :varying ; ~:@/.../
                   :uniform) ; ~:/.../
               (if atp
                   :out ; ~@/.../
                   :in)))) ; ~/.../
      ;; Header.
      (ecase style
        (:varying (write-string "varying " out))
        (:uniform (write-string "uniform " out))
        (:out (write-string "out " out))
        (:in
         (funcall (formatter "~@[layout (location = ~D) ~]in ") out
                  (first info))))
      ;; Type.
      (let ((type (third info)))
        (if (not (glsl-structure-name-p type))
            (glsl-symbol out type)
            (ecase style
              ((:in :out)
               (funcall (formatter "~A {~4I~:@_") out
                        (change-case:pascal-case (symbol-name type)))
               (loop :for (slot . rest)
                          :on (c2mop:class-slots
                                (c2mop:ensure-finalized (find-class type)))
                     :do (funcall
                           (formatter
                            "~/fude-gl:glsl-symbol/ ~/fude-gl:glsl-symbol/;")
                           out (glsl-type slot)
                           (c2mop:slot-definition-name slot))
                         (if rest
                             (funcall (formatter "~:@_") out)
                             (funcall (formatter "~I~:@_}") out))))
              ((:uniform :varying)
               (write-string (change-case:pascal-case (symbol-name type))
                             out)))))
      ;;
      (funcall (formatter " ~A~@[[~A]~];") out
               ;; var
               (symbol-camel-case (second info))
               ;; array?
               (let ((vector-size (fourth info)))
                 (etypecase vector-size
                   (null nil)
                   ((eql t) "")
                   (index vector-size)
                   (constant-definition
                    (change-case:constant-case
                      (symbol-name (second vector-size))))))))))

(defun attribute-decls (attributes)
  (loop :for class-name :in attributes
        :for slots = (c2mop:class-direct-slots (find-class class-name))
        :for i :of-type (mod #.most-positive-fixnum) :upfrom 0
        :when slots
          :collect `(type
                     ,(let ((length (length (the list slots))))
                        (ecase length
                          (1 :float)
                          ((2 3 4)
                           (intern (format nil "VEC~D" length) :keyword))))
                     ,class-name)
        :collect `(glsl-env:notation ,class-name
                   ,(symbol-camel-case class-name))
        :collect `(location ,class-name ,i)
        :collect `(attribute ,class-name :attribute)))

(defun attribute-ins (decls)
  (let ((table (make-hash-table)))
    (loop :for decl :in decls
          :when (eq 'type (car decl))
            :do (setf (gethash (third decl) table)
                        (list (third decl) (second decl)))
          :when (eq 'location (car decl))
            :do (push (third decl) (gethash (second decl) table)))
    (sort (the list (alexandria:hash-table-values table)) #'< :key #'car)))

(defun io-decl (out-specs)
  (uiop:while-collecting (acc)
    (loop :for (nil var type . vector-size) :in out-specs
          :do (acc
               `(type
                 ,(if vector-size
                      `(vector ,type ,(car vector-size))
                      type)
                 ,var))
              (acc `(glsl-env:notation ,var ,(symbol-camel-case var)))
          :if (glsl-structure-name-p type)
            :do (mapc #'acc (slot-reader-decls type var)))))

(defun constant-decl (constant-defs)
  (loop :for (nil name value) :in constant-defs
        :collect `(eprot::constant ,name ,value)
        :collect `(glsl-env:notation ,name
                   ,(change-case:constant-case (symbol-name name)))))

(defun canonicalize-uniform-specs (specs)
  (loop :for spec :in specs
        :if (listp spec) ; ignore uniform-buffer-object.
          :collect (cons nil spec)))

(defun glsl-ubo (out ubo &rest noise)
  (declare (ignore noise))
  (let ((ubo (class-of ubo)))
    (pprint-logical-block (out nil)
      ;; header
      (funcall (formatter "layout (~(~A~)) uniform ~A~:@_") out (layout ubo)
               (change-case:pascal-case (symbol-name (class-name ubo))))
      ;; begin block.
      (funcall (formatter "{~4I~:@_") out)
      ;; Each slot.
      (loop :for (slot . rest)
                 :on (c2mop:class-slots (c2mop:ensure-finalized ubo))
            :do (funcall
                  (formatter "~/fude-gl:glsl-symbol/ ~/fude-gl:glsl-symbol/;")
                  out (glsl-type slot) (c2mop:slot-definition-name slot))
                (and rest (funcall (formatter " ~:@_") out)))
      ;; end block.
      (funcall (formatter "~I~:@_};") out))))

(defun parse-main (main)
  "Canonicalize ftype (function (type) type) to (function ((var type)) type)."
  (let ((table (make-hash-table :test #'eq)))
    (multiple-value-call #'append
      (uiop:while-collecting (constants ftypes defuns)
        (dolist (sexp main)
          (case (car sexp)
            (defun (push sexp (gethash (cadr sexp) table)))
            (defconstant (constants sexp))
            (declaim
             (dolist (declaration (cdr sexp))
               (case (car declaration)
                 (ftype
                  (dolist (name (cddr declaration))
                    (push (cadr declaration) (gethash name table))))
                 (type (constants `(declaim ,declaration)))
                 (otherwise (error "PARSE-MAIN: NIY ~S." declaration)))))
            (otherwise (error "PARSE-MAIN: NIY ~S." sexp))))
        (maphash
          (lambda (name forms)
            (let ((defun (assoc 'defun forms)) (ftype (assoc 'function forms)))
              (if (null defun)
                  (if ftype
                      (error "Missing definition. ~S" ftype)
                      (error "PARSE-MAIN: NIY ~S" name))
                  (if (null ftype)
                      (error "FTYPE declaration is required. ~S" defun)
                      (progn
                       (ftypes
                        `(declaim
                          (ftype (function
                                  ,(mapcar #'list (third defun) (second ftype))
                                  ,(third ftype))
                                 ,name)))
                       (defuns defun))))))
          table)))))

(defun pprint-layout-output-spec (out spec &rest noise)
  (declare (ignore noise))
  (pprint-logical-block (out nil :prefix "(" :suffix ")")
    (write-string (change-case:snake-case (symbol-name (car spec))) out)
    (when (cdr spec)
      (write-char #\, out)
      (write-char #\Space out)
      (loop :for ((key value) . rest) :on (cdr spec)
            :do (funcall (formatter "~A = ~A") out
                         (change-case:snake-case (symbol-name key)) value)
            :when rest
              :do (write-char #\, out)
                  (write-char #\Space out)))))

(defun geometry-io-declaration (out io &rest noise)
  (declare (ignore noise))
  (pprint-logical-block (out nil)
    ;; in
    (funcall (formatter "layout (~A) in;~:@_") out
             (change-case:snake-case (symbol-name (car io))))
    ;; out
    (pprint-logical-block (out nil)
      (funcall
        (formatter "layout ~/fude-gl:pprint-layout-output-spec/ out;~:@_") out
        (cadr io)))))

(defun <shader-forms> (shader-clause* superclasses name version)
  (let* ((format
          (formatter
           #.(concatenate 'string "#version ~A core~%" ; version
                          "~@[~{~/fude-gl:glsl-constant-definition/~%~}~]" ; define
                          "~@[~/fude-gl:geometry-io-declaration/~]" ; geometry-io
                          "~{~/fude-gl:glsl-declaration/~%~}~&" ; in
                          "~{~/fude-gl:glsl-ubo/~%~}~&" ; ubo
                          "~{~@/fude-gl:glsl-declaration/~%~}~&" ; out
                          "~@[~{~/fude-gl:glsl-struct-definition/~}~]" ; struct defs.
                          "~@[~{~:/fude-gl:glsl-declaration/~%~}~]~&" ; uniforms
                          "~@[~{~:@/fude-gl:glsl-declaration/~%~}~]~&" ; varying.
                          "~@[~{~/fude-gl:pprint-glsl/~^~}~]" ; functions.
                          )))
         (attribute-decls (attribute-decls superclasses))
         (eprot:*environment*
          (eprot:augment-environment (eprot:find-environment :fude-gl)
                                     :variable superclasses
                                     :declare attribute-decls)))
    (labels ((rec (shaders in varying acc)
               (if (endp shaders)
                   (nreverse acc)
                   (body (car shaders) (cdr shaders) in varying acc)))
             (body (shader rest in varying acc)
               (destructuring-bind
                   (type shader-lambda-list &rest main)
                   shader
                 (cond
                   ((typep main
                           '(cons (and symbol (satisfies find-shader)) null))
                    (inherit rest varying acc type (car main)))
                   ((every #'consp main)
                    (own rest in varying acc type shader-lambda-list main))
                   (t (error "<SHADER-FORMS>: NIY ~S." main)))))
             (inherit (rest varying acc type source)
               (multiple-value-bind (out uniform varying%)
                   (split-shader-lambda-list (shader-lambda-list source type))
                 (let* ((out (mapcar (lambda (out) (cons nil out)) out))
                        (varying
                         (mapcar (lambda (out) (cons nil out)) varying))
                        (eprot:*environment*
                         (eprot:augment-environment eprot:*environment*
                                                    :variable (mapcar #'cadr
                                                                      out)
                                                    :declare (io-decl out))))
                   (dolist (spec uniform) ; set related shaders.
                     (let ((ubo (uniform-buffer-object-name-p spec)))
                       (when ubo
                         (push name (related-shaders (class-of ubo))))))
                   (rec rest out (append varying varying%)
                        (let ((method
                               (intern (format nil "~A-SHADER" type) :fude-gl)))
                          (cons
                            `(defmethod ,method ((type (eql ',name)))
                               (,method ',source))
                            acc))))))
             (own (rest in varying acc type shader-lambda-list main)
               (declare (list shader-lambda-list))
               (when (eq :geometry type)
                 (setq in
                         (mapcar
                           (lambda (info)
                             (destructuring-bind
                                 (layout-loc var type)
                                 info
                               (if (glsl-structure-name-p type)
                                   (list layout-loc var type t)
                                   info)))
                           in)))
               (multiple-value-bind (out uniform varying%)
                   (split-shader-lambda-list
                     (if (eq :geometry type)
                         (cddr shader-lambda-list)
                         shader-lambda-list))
                 (let* ((constant-defs (constant-defs uniform))
                        (struct-defs (struct-defs uniform))
                        (out (mapcar (lambda (out) (cons nil out)) out))
                        (ubo
                         (mapcan
                           (lambda (spec)
                             (let ((ubo (uniform-buffer-object-name-p spec)))
                               (when ubo
                                 (push name (related-shaders (class-of ubo)))
                                 (list ubo))))
                           uniform))
                        (ubos
                         (loop :for instance :in ubo
                               :nconc (mapcar
                                        (lambda (slot)
                                          (list nil
                                                (c2mop:slot-definition-name
                                                  slot)
                                                (glsl-type slot)))
                                        (c2mop:class-slots
                                          (c2mop:ensure-finalized
                                            (class-of instance))))))
                        (uniform (canonicalize-uniform-specs uniform))
                        (varying
                         (mapcar (lambda (out) (cons nil out)) varying))
                        (eprot:*environment*
                         (eprot:augment-environment eprot:*environment*
                                                    :variable (mapcar #'cadr
                                                                      out)
                                                    :declare (io-decl out))))
                   (rec rest out (append varying varying%)
                        (cons
                          (let ((method
                                 (intern (format nil "~A-SHADER" type)
                                         :fude-gl))
                                (eprot:*environment*
                                 (eprot:augment-environment eprot:*environment*
                                                            :name type
                                                            :variable (mapcar
                                                                        #'cadr
                                                                        (append
                                                                          uniform
                                                                          ubos
                                                                          varying%
                                                                          constant-defs))
                                                            :function (struct-readers
                                                                        struct-defs)
                                                            :declare (append
                                                                       (io-decl
                                                                         uniform)
                                                                       (io-decl
                                                                         ubos)
                                                                       (io-decl
                                                                         varying%)
                                                                       (constant-decl
                                                                         constant-defs)))))
                            `(defmethod ,method ((type (eql ',name)))
                               ,(format nil format version constant-defs
                                        (when (eq :geometry type)
                                          (subseq shader-lambda-list 0 2))
                                        in ubo out struct-defs uniform
                                        (append varying varying%)
                                        (parse-main main))))
                          acc))))))
      (rec shader-clause* (attribute-ins attribute-decls) nil nil))))

(defun <shader-lambda-lists> (name shader*)
  (loop :for (type lambda-list . main) :in shader*
        :collect `(defmethod shader-lambda-list
                             ((shader-name (eql ',name))
                              (shader-type (eql ,type)))
                    ,(if (typep main
                                '(cons (and symbol (satisfies find-shader))
                                       null))
                         `(shader-lambda-list ',(car main) ,type)
                         `',lambda-list))))

(defmacro defshader
          (&whole whole name version (&rest attribute*) &body shader*)
  (check-bnf:check-bnf (:whole whole)
    ((name symbol))
    ((version (mod #.most-positive-fixnum)))
    ((attribute* (satisfies vertex-attribute-p)))
    ((shader* (or vertex-clause fragment-clause geometry-clause))
     ;;
     (vertex-clause ((eql :vertex) shader-lambda-list main*))
     ;;
     (fragment-clause ((eql :fragment) shader-lambda-list main*))
     ;;
     (geometry-clause ((eql :geometry) geometry-lambda-list main*))
     ;;
     (shader-lambda-list
      (out-spec* uniform-keyword? uniform-spec* varying-keyword?
       varying-spec*))
     (out-spec (var type-spec))
     (uniform-keyword (satisfies uniform-keywordp))
     (uniform-spec (or (var type-spec vector-size?) ubo-spec))
     (ubo-spec (satisfies uniform-buffer-object-name-p))
     (vector-size (or index constant-definition))
     (varying-keyword? (satisfies varying-keywordp))
     (varying-spec (var type-spec))
     ;;
     (geometry-lambda-list
      (layout-input-primitive layout-spec out-spec* uniform-keyword?
       uniform-spec*))
     (layout-spec (layout-output-primitive layout-output-option*))
     (layout-output-option (symbol fixnum))
     ;;
     (main check-bnf:expression)))
  ;; The body.
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name *shaders*)
             (defclass ,name ,attribute* () (:metaclass vector-class)))
     ,@(<shader-forms> shader* attribute* name version)
     ,(<uniforms> name (remove :geometry shader* :key #'car))
     ,@(<shader-lambda-lists> name shader*)
     ',name))

(defmethod documentation ((this (eql 'defshader)) (type (eql 'function)))
  "Define lisp side meta shader protocol object and its generic function methods.
You can define GLSL vertex shader code generator (i.e. generic-function VERTEX-SHADER) and/or
GLSL fragment shader code generator (i.e. generic-function FRAGMENT-SHADER) and
vertex constructor (i.e. make-instance NAME.) via this macro.
Vertex constructor makes a single-float vector that's length depends on its ATTRIBUTEs.")

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

(set-pprint-dispatch '(cons (member defshader)) 'pprint-defshader)

;;;;
;;;; CONSTRUCT

(defun parse-log (log)
  "Return two values.
  1. Boolean: Has index?
  2. List of integers (start line-index token-index) if first value is true.
     List of string if first value is nil."
  (let* ((tokens (ppcre:split ": ?" log))
         (position (position "error" tokens :test #'equal))
         (pre-error (and position (subseq tokens 0 position))))
    (declare (list tokens))
    (ecase (length pre-error)
      (0 (values nil nil))
      (2
       (destructuring-bind
           (start? line-index?)
           pre-error
         (let ((positions
                (delete ""
                        (the list
                             (uiop:split-string line-index? :separator "()"))
                        :test #'equal)))
           (declare (list tokens))
           (if (and (every #'digit-char-p start?)
                    (= 2 (length positions))
                    (every #'digit-char-p (car positions))
                    (every #'digit-char-p (cadr positions)))
               (values t
                       (list (parse-integer start?)
                             (parse-integer (car positions))
                             (parse-integer (cadr positions))))
               (values nil (list start? line-index?)))))))))

(define-condition glsl-compile-error (fude-gl-error cell-error)
  ((origin :initarg :origin :reader origin)
   (source :initarg :source :reader source))
  (:report
   (lambda (this out)
     (format out "~@[Fail to compile ~S~:@_~]~A~:@_" (cell-error-name this)
             (origin this))
     (multiple-value-bind (has-index? args)
         (parse-log (origin this))
       (let ((source (source this)))
         (if (not has-index?)
             (format out "Source ~S" source)
             (destructuring-bind
                 (start line-index token-index)
                 args
               (declare (ignore start token-index)
                        (fixnum line-index))
               (with-input-from-string (in source)
                 (loop :for line = (read-line in nil nil)
                       :for index :of-type fixnum :upfrom 1
                       :while line
                       :if (< index line-index)
                         :do (write-line line out)
                       :else
                         :do (cl-ansi-text:with-color (:red :stream out)
                               (write-line line out)))))))))))

(defun compile-shader
       (program-id vertex-shader fragment-shader geometry-shader)
  "Request openGL to compile and link shader programs. Return nil."
  (let ((vs (gl:create-shader :vertex-shader))
        (fs (gl:create-shader :fragment-shader))
        (gs (and geometry-shader (gl:create-shader :geometry-shader))))
    (unwind-protect
        (labels ((compile-s (program-id id source)
                   (gl:shader-source id source)
                   (gl:compile-shader id)
                   (may-warn (gl:get-shader-info-log id) source)
                   (gl:attach-shader program-id id))
                 (may-warn (log source)
                   (declare (string log)
                            #+sbcl ; due to not simple-string.
                            (sb-ext:muffle-conditions sb-ext:compiler-note))
                   (unless (equal "" log)
                     (if (search "error:" log)
                         (error 'glsl-compile-error :origin log :source source)
                         (warn log)))))
          (compile-s program-id vs vertex-shader)
          (compile-s program-id fs fragment-shader)
          (and gs (compile-s program-id gs geometry-shader))
          (gl:link-program program-id)
          (may-warn (gl:get-program-info-log program-id) program-id))
      (protect (gl:delete-shader fs))
      (protect (gl:delete-shader vs))
      (and gs (protect (gl:delete-shader gs))))))

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
    (handler-bind ((glsl-compile-error
                    (lambda (c) (reinitialize-instance c :name shader))))
      (compile-shader program (vertex-shader shader) (fragment-shader shader)
                      (geometry-shader shader)))
    program))

(defmethod construct ((o vector-class))
  (setf (program-id o) (create-program (class-name o)))
  o)

;;;; DESTRUCT

(defmethod destruct ((o vector-class))
  (gl:delete-program (program-id o))
  (setf (program-id o) nil))

;;;; FIND-PROGRAM

(defun find-program (name &key (if-does-not-exist :error))
  "Return program ID of the NAME if exists, otherwise depends on IF-DOES-NOT-EXIST."
  (let ((instance (find-shader name nil)))
    (or (and instance (program-id instance))
        (ecase if-does-not-exist
          (:error (error 'missing-program :name name))
          (:create (program-id (construct (find-shader name))))
          ((nil) nil)))))

;;;; IN-PROGRAM

(define-condition glsl-program-error (fude-gl-error cell-error)
  ((origin :initarg :origin :reader origin))
  (:report
   (lambda (this out)
     (format out "~A~:@_~S: ~S" (origin this) 'in-program
             (cell-error-name this)))))

(defmacro in-program (name)
  (when (constantp name)
    (find-shader (eval name)))
  (let ((?name (gensym "NAME")))
    `(let ((,?name ,name))
       (handler-case
           (gl:use-program (find-program ,?name :if-does-not-exist :create))
         (cl-opengl-bindings:opengl-error (c)
           (error 'glsl-program-error :origin c :name ,?name))))))

;;;; UNIFORM

(define-condition uniform-error (fude-gl-error cell-error)
  ((shader :initarg :shader :reader shader)))

(define-condition missing-uniform (uniform-error)
  ()
  (:report
   (lambda (this output)
     (format output "Missing uniform named ~S. ~:@_" (cell-error-name this))
     (did-you-mean output (cell-error-name this)
                   (mapcar #'uniform-name (uniforms (shader this))))
     (format output " ~:@_To see all supported uniforms, evaluate ~S."
             `(uniforms ',(shader this))))))

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

(define-compiler-macro uniform (&whole whole shader name)
  (check-uniform-args shader name)
  whole)

(define-condition non-active-uniform (uniform-error)
  ()
  (:report
   (lambda (this output)
     (format output
             "Uniform ~S is not active in shader ~S. ~:@_Active uniforms in GL are ~S."
             (cell-error-name this) (shader this)
             (gl:get-program (program-id (find-shader (shader this)))
                             :active-uniforms)))))

(define-condition uniform-mismatch-ubo (uniform-error)
  ()
  (:report
   (lambda (this out)
     (format out
             "Could not get uniform location for uniform buffer object. ~S ~S"
             (cell-error-name this) (shader this)))))

(defun uniform (shader name)
  "Return the uniform location of NAME in the SHADER."
  (let ((location
         (gl:get-uniform-location (program-id (find-shader shader)) name)))
    (declare ((signed-byte 32) location))
    (when (minusp location)
      (let ((uniform
             (find name (the list (uniforms shader))
                   :test #'equal
                   :key #'uniform-name)))
        (cond
          ((not uniform) (error 'missing-uniform :name name :shader shader))
          ((eq 'uniform-buffer-object (uniform-lisp-type uniform))
           (error 'uniform-mismatch-ubo :name name :shader shader))
          (t (error 'non-active-uniform :name name :shader shader)))))
    location))

(define-setf-expander uniform (shader name &rest args)
  (check-uniform-args shader name)
  (let ((new (gensym "NEW")))
    (values nil
            nil
            (list new)
            `(progn (send ,new ,shader :uniform ,name ,@args) ,new))))

;;;; WITH-UNIFORMS

(defmacro with-uniforms ((&rest var*) shader &body body &environment env)
  (flet ((parse-var-spec (spec)
           (etypecase spec
             (symbol (values spec (symbol-camel-case spec) nil))
             ((cons symbol (cons string t))
              (values (car spec) (cadr spec) (cddr spec)))
             ((cons symbol (cons keyword t))
              (values (car spec) (symbol-camel-case (car spec)) (cdr spec)))
             ((cons symbol (cons symbol t))
              (values (car spec)
                      (symbol-camel-case (cadr spec))
                      (cddr spec))))))
    ;; Trivial-syntax-check
    (when (constantp shader env)
      (dolist (var var*)
        (check-uniform-args shader (nth-value 1 (parse-var-spec var)))))
    (let ((s (gensym "SHADER"))
          (uniforms
           (when (constantp shader env)
             (uniforms (eval shader)))))
      (declare (list uniforms))
      (flet ((<macrolet-bind> (spec)
               (multiple-value-bind (symbol name option)
                   (parse-var-spec spec)
                 (let ((uniform
                        (find name uniforms :test #'equal :key #'uniform-name)))
                   (if uniform
                       `(,symbol
                         (the ,(uniform-lisp-type uniform)
                              (uniform ,s ,name ,@option)))
                       `(,symbol (uniform ,s ,name ,@option)))))))
        `(let ((,s ,shader))
           (symbol-macrolet ,(mapcar #'<macrolet-bind> var*)
             ,@body))))))

(defmethod documentation ((o (eql 'with-uniforms)) (type (eql 'function)))
  "Evaluate the BODY in the context that each VAR is bound by the uniform of the SHADER.

VAR := [ symbol | (symbol alias? { :unit unsigned-byte }?) ]
ALIAS := [ (and symbol (not (or keyword boolean))) | string ]
SHADER := The form that generates a shader name symbol.
BODY := Implicit progn.

NOTE: Each VAR is SETFable.
NOTE: When ALIAS is a symbol, it will be filtered by symbol-camel-case to generate a uniform name.")

;;;; SEND

(define-compiler-macro send
                       (&whole whole object to &key uniform &allow-other-keys)
  (declare (ignore object))
  ;; Compile time uniform existance check.
  (and uniform (check-uniform-args to uniform))
  whole)

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

(defmethod send ((o 3d-matrices:mat4) (to uniform-buffer-object) &key uniform)
  (let* ((buffer (buffer (class-of to)))
         (slots (c2mop:class-slots (class-of to)))
         (slot
          (find uniform slots
                :test #'equal
                :key (alexandria:compose #'symbol-camel-case
                                         #'c2mop:slot-definition-name)))
         (gl-vector (slot-value to (c2mop:slot-definition-name slot))))
    (declare (list slots))
    #+sbcl ; Not our responsebilities
    (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    ;; Update gl-vector
    (loop :for i :upfrom 0
          :for elt :across (3d-matrices:marr o)
          :do (setf (gl:glaref gl-vector i) elt))
    (in-buffer buffer)
    (gl:buffer-sub-data (buffer-target buffer) gl-vector
                        :buffer-offset (reduce #'+ slots
                                               :end (position slot slots
                                                              :test #'eq)
                                               :key (lambda (slot)
                                                      (gl:gl-array-byte-size
                                                        (slot-value to
                                                                    (c2mop:slot-definition-name
                                                                      slot))))))))

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

(defmethod send
           ((o cons) (to symbol)
            &key (uniform (alexandria:required-argument :uniform)))
  (loop :for elt :in o
        :for i :of-type fixnum :upfrom 0
        :do (send elt to :uniform (format nil "~A[~D]" uniform i))))

;;;; GENERIC FUNCTIONS

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