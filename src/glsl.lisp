(in-package :fude-gl)

;;;; GLSL-STRUCTURE

(defclass glsl-structure-class (standard-class) ()
  (:documentation "Meta class for glsl structure."))

(defmethod c2mop:validate-superclass
           ((s glsl-structure-class) (c standard-class))
  t)

(defclass glsl-slot-mixin ()
  ((glsl-type :type keyword :initarg :glsl-type :reader glsl-type))
  (:documentation "Meta informations for the slot of the glsl structures."))

(defclass glsl-direct-slot-definition (c2mop:standard-direct-slot-definition glsl-slot-mixin)
  ())

(defmethod c2mop:direct-slot-definition-class
           ((s glsl-structure-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'glsl-direct-slot-definition))

(defclass glsl-effective-slot (c2mop:standard-effective-slot-definition glsl-slot-mixin)
  ())

(defmethod c2mop:effective-slot-definition-class
           ((s glsl-structure-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'glsl-effective-slot))

(defmethod c2mop:compute-effective-slot-definition
           ((o glsl-structure-class) slot-name direct-slot-definitions)
  (let ((effective-slot (call-next-method)))
    (setf (slot-value effective-slot 'glsl-type)
            (glsl-type
              (or (find (the symbol slot-name)
                        (the list direct-slot-definitions)
                        :key #'c2mop:slot-definition-name)
                  (error "Missing slot named ~S in ~S." slot-name
                         direct-slot-definitions))))
    effective-slot))

(defclass glsl-structure-object () ())

(defmacro define-glsl-structure (name () (&rest slot-spec*) &body option*)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defclass ,name (glsl-structure-object) ,slot-spec*
       (:metaclass glsl-structure-class)
       ,@option*)))

(defun pprint-define-glsl-structure (output form &rest noise)
  (declare (ignore noise))
  (funcall
    (formatter
     #.(concatenate 'string "~:<" ; pprint-logical-block.
                    "~W~^ ~1I~@_" ; operator.
                    "~W~^ ~@_" ; name.
                    "~:<~@{~W~^ ~:_~}~:>~^ ~_" ; super-clausses.
                    "~:<~@{~:<~W~^ ~:I~@{~W~^ ~@_~W~^ ~_~}~:>~^ ~_~}~:>~^ ~_" ; slots
                    "~@{~W~^ ~_~}" ; options
                    "~:>"))
    output form))

(set-pprint-dispatch '(cons (member define-glsl-structure))
                     'pprint-define-glsl-structure)

(defun glsl-structure-name-p (thing)
  (and (symbolp thing) (typep (find-class thing nil) 'glsl-structure-class)))

(defun glsl-structure-initargs (glsl-structure-name)
  (loop :for slot
             :in (c2mop:class-slots
                   (c2mop:ensure-finalized (find-class glsl-structure-name)))
        :append (c2mop:slot-definition-initargs slot)))

(define-condition fude-gl-error (error) ())

(define-condition shader-error (fude-gl-error program-error) ())

(defun did-you-mean (out string selection)
  (setq out (or out *standard-output*))
  (apply #'format out "Did you mean ~#[~;~S~;~S or ~S~:;~S, ~S, or ~S~] ?"
         (fuzzy-match:fuzzy-match string selection)))

(define-condition unknown-initarg (fude-gl-error cell-error)
  ((glsl-structure :initarg :glsl-structure :reader glsl-structure))
  (:report
   (lambda (this out)
     (format out "Unknown initarg ~S for glsl structure ~S. ~:@_"
             (cell-error-name this) (glsl-structure this))
     (did-you-mean out (symbol-name (cell-error-name this))
                   (glsl-structure-initargs (glsl-structure this)))
     (format out " ~:@_To see all supported initargs, evaluate ~S."
             `(glsl-structure-initargs ',(glsl-structure this))))))

(defun check-initarg-existence (glsl-structure-name arg-forms env)
  (let ((initargs (class-initargs (find-class glsl-structure-name))))
    (loop :for form :in arg-forms :by #'cddr
          :when (and (constantp form env) (not (find (eval form) initargs)))
            :do (error 'unknown-initarg
                       :name (eval form)
                       :glsl-structure glsl-structure-name))))

(define-compiler-macro make-object
                       (&whole whole name &rest arg-forms &environment env)
  (when (constantp name env)
    (check-initarg-existence (eval name) arg-forms env))
  whole)

(defun make-object (name &rest args) (apply #'make-instance name args))

;;;; ENVIRONMENT

(defstruct environment
  (variable nil :type list :read-only t)
  (function nil :type list)
  (next nil :type (or null environment) :read-only t))

(defmethod print-object ((o environment) output)
  (print-unreadable-object (o output :type t :identity t)))

(defvar *environment*
  (make-environment :next nil
                    :variable glsl-spec:*variables*
                    :function (append glsl-spec:*functions*
                                      glsl-spec:*operators*
                                      glsl-spec:*vector-constructors*
                                      glsl-spec:*matrix-constructors*)))

(deftype glsl-type ()
  '(member :bool :int :uint :float :vec2 :vec3 :vec4 :uvec3 :mat4 :|sampler2D|))

(defun variable-information-var (info) (getf info :lisp-name))

(defun variable-information-name (info) (getf info :name))

(defun variable-information-type (info)
  (if (getf info :versions)
      :global
      (getf info :attribute)))

(defun variable-information-glsl-type (info) (getf info :type))

(defun variable-information-ref? (info) (getf info :ref?))

(defun (setf variable-information-ref?) (new info)
  (let ((sentinel '#:sentinel))
    (if (eq sentinel (getf info :ref? sentinel))
        (nconc info (list :ref? new))
        (setf (getf info :ref?) new)))
  new)

(defun make-variable-information (&key var name type glsl-type)
  (list :lisp-name (symbol-name var)
        :name name
        :type glsl-type
        :attribute type))

(defun variable-information (symbol &optional env)
  (let* ((global?
          (cond ((uiop:string-prefix-p "gl_" symbol) (symbol-name symbol))
                ((uiop:string-prefix-p "GL-" symbol)
                 (let ((name
                        (format nil "gl_~A"
                                (change-case:pascal-case
                                  (subseq (symbol-name symbol) 3)))))
                   name))))
         (name (or global? (symbol-name symbol)))
         (key
          (if global?
              #'variable-information-name
              #'variable-information-var)))
    (labels ((rec (env)
               (unless (null env)
                 (or (find name (environment-variable env)
                           :test #'equal
                           :key key)
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
                 (delete-duplicates acc :test #'equal)
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
    (let ((name (symbol-name var)))
      (labels ((rec (env)
                 (unless (null env)
                   (let ((info
                          (find name (environment-variable env)
                                :test #'equal
                                :key #'variable-information-var)))
                     (if info
                         (when (not (variable-information-ref? info))
                           (with-standard-io-syntax
                            (warn 'unused-variable :name var)))
                         (rec (environment-next env)))))))
        (rec *environment*)))))

(defun function-information (symbol &optional env)
  (let ((name (symbol-name symbol)))
    (labels ((rec (env)
               (unless (null env)
                 (or (remove-if-not
                       (lambda (spec) (equal name (getf spec :lisp-name)))
                       (environment-function env))
                     (rec (environment-next env))))))
      (rec env))))

(defun list-all-known-functions ()
  (labels ((rec (env acc)
             (if (null env)
                 (delete-duplicates acc :test #'equal)
                 (rec (environment-next env)
                      (progn
                       (loop :for spec :in (environment-function env)
                             :do (push (getf spec :name) acc))
                       acc)))))
    (rec *environment* nil)))

(defun struct-readers (struct-names)
  (uiop:while-collecting (acc)
    (dolist (name struct-names)
      (dolist
          (slot
           (c2mop:class-direct-slots
             (c2mop:ensure-finalized (find-class name))))
        (dolist (reader (c2mop:slot-definition-readers slot))
          (acc
           (list :lisp-name (symbol-name reader)
                 :name (symbol-camel-case (c2mop:slot-definition-name slot))
                 :return (glsl-type slot)
                 :attribute :reader)))))))

(defun argument-environment (env &key variable function)
  (make-environment :next env :variable variable :function function))

(defun print-signatures (infos &optional (stream *standard-output*))
  (format stream "~<~@{~:<FUNCTION ~W ~W~:>~^ ~:@_~}~:>"
          (mapcar (lambda (info) (list (getf info :args) (getf info :return)))
                  infos))
  (force-output stream))

;;;; GLSL PRINTERS

(defvar *cl-pp-dispatch* (copy-pprint-dispatch nil))

(defmacro with-cl-io-syntax (&body body)
  `(let ((*print-pprint-dispatch* *cl-pp-dispatch*))
     ,@body))

(defmacro with-hint ((&rest bind*) &body body)
  `(restart-bind ,(mapcar
                    (lambda (bind)
                      (destructuring-bind
                          (report form)
                          bind
                        `(hint
                          (lambda ()
                            (let ((*print-length*))
                              (print ,form *debug-io*)))
                          :report-function
                          (lambda (s) (write-string ,report s)))))
                    bind*)
     ,@body))

(defun symbol-camel-case (s) (change-case:camel-case (symbol-name s)))

(defvar *var-check-p* nil)

(define-condition unknown-variable (shader-error cell-error)
  ((known-vars :initarg :known-vars :reader known-vars))
  (:report
   (lambda (this output)
     (pprint-logical-block (output nil)
       (format output "Unknown variable. ~S ~:@_" (cell-error-name this))
       (did-you-mean output (symbol-camel-case (cell-error-name this))
                     (known-vars this))))))

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
otherwise the condition is not signaled. The default is NIL.
You can specify this by colon in format control.
If EXP is known for compiler and NOT-REF-P is ture, compiler memos it is refered
otherwise compiler do nothing. The default it NIL. You can specify this by at-sign in format control."
  (let ((info (variable-information exp *environment*)))
    (cond
      (info
       (unless not-ref-p ; means refered-p
         (unless (eq :global (variable-information-type info))
           (setf (variable-information-ref? info) t)))
       (write-string (variable-information-name info) stream))
      ((and errorp
            (with-cl-io-syntax
              (cerror "Anyway print it." 'unknown-variable
                      :name exp
                      :known-vars (list-all-known-vars)))))
      ((find-if #'lower-case-p (symbol-name exp))
       (write-string (symbol-name exp) stream))
      (t (write-string (symbol-camel-case exp) stream)))))

(defun glsl-setf (stream exp)
  (setf stream (or stream *standard-output*))
  (let ((*var-check-p* t))
    (funcall (formatter "~{~:/fude-gl::glsl-symbol/ = ~W;~:@_~}") stream
             (cdr exp))))

(defun class-readers (class-name)
  (loop :for slot :in (c2mop:class-direct-slots (find-class class-name))
        :append (c2mop:slot-definition-readers slot)))

(define-condition unknown-glsl-function (fude-gl-error cell-error)
  ((form :initarg :form :reader form))
  (:report
   (lambda (this output)
     (format output "Unknown glsl function named ~S. ~:@_"
             (cell-error-name this))
     (did-you-mean output (symbol-camel-case (cell-error-name this))
                   (list-all-known-functions))
     (format output " ~:@_To see all supported glsl functions, evaluate ~S."
             '(list-all-known-functions))
     (format output "~@[ ~:@_~?~]"
             (let ((info
                    (when (typep (form this) '(cons symbol (cons symbol null)))
                      (variable-information (cadr (form this)) *environment*))))
               (when (and info (glsl-structure-name-p (getf info :type)))
                 "Or slot reader may missing. To see all supported slot readers, evaluate ~S."))
             (list `(class-readers ',(cadr (form this))))))))

(define-condition glsl-argument-mismatch (fude-gl-error)
  ((form :initarg :form :reader form))
  (:report
   (lambda (this out)
     (format out "Arguments for ~S does not match its signature. ~S~@[~?~]"
             (car (form this)) (form this)
             (let ((infos
                    (function-information (car (form this)) *environment*)))
               (unless (find-if (lambda (info) (getf info :attribute)) infos)
                 " ~:@_For detail, evaluate ~S."))
             (list
               `(function-information ',(car (form this)) *environment*))))))

(defun glsl-swizzling (stream exp)
  (setf stream (or stream *standard-output*))
  (unless (= 1 (length (cdr exp)))
    (with-cl-io-syntax
      (error 'glsl-argument-mismatch :form exp)))
  (let ((*var-check-p* t))
    (format stream "~W.~/fude-gl:glsl-symbol/" (cadr exp) (car exp))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; To muffle the compiler claiming undefined variable.
  (unless (boundp '+swizzling+)
    (defconstant +swizzling+
      (labels ((swizzling (components)
                 (append (rec components 1 nil) (rec components 2 nil)
                         (rec components 3 nil) (rec components 4 nil)))
               (rec (components depth acc)
                 (if (= 1 depth)
                     (loop :for c :in components
                           :collect (format nil "~{~A~}" (cons c acc)))
                     (loop :for c :in components
                           :append (rec components (1- depth) (cons c acc))))))
        (uiop:list-to-hash-set
          (append (swizzling '(x y z w)) (swizzling '(r g b a))
                  (swizzling '(s t u v))))))))

(defun glsl-slot-reader (stream exp info)
  (setf stream (or stream *standard-output*))
  (pprint-logical-block (stream nil)
    (write (cadr exp) :stream stream)
    (write-char #\. stream)
    (write-string (getf info :name) stream)))

(defun glsl-funcall (stream exp)
  (setf stream (or stream *standard-output*))
  (if (gethash (symbol-name (car exp)) +swizzling+)
      (glsl-swizzling stream exp)
      (let ((info (function-information (car exp) *environment*)))
        (unless info
          (with-cl-io-syntax
            (with-hint (("Print all known functions."
                         (list-all-known-functions)))
              (cerror "Anyway, print it." 'unknown-glsl-function
                      :name (car exp)
                      :form exp))))
        (let ((reader-info
               (find-if (lambda (info) (eq :reader (getf info :attribute)))
                        info)))
          (if reader-info
              (if (= 1 (length (cdr exp)))
                  (glsl-slot-reader stream exp reader-info)
                  (with-cl-io-syntax
                    (with-hint (("Print function informations." info))
                      (error 'glsl-argument-mismatch :form exp))))
              (if (or (null info) ; continued.
                      (and info
                           (find (length (cdr exp)) info
                                 :key (lambda (info)
                                        (length (getf info :args))))))
                  (funcall
                    (formatter "~/fude-gl:glsl-symbol/~:<~@{~W~^, ~@_~}~:>")
                    stream (car exp) (cdr exp))
                  (with-cl-io-syntax
                    (with-hint (("Print function signatures."
                                 (print-signatures info)))
                      (error 'glsl-argument-mismatch :form exp)))))))))

(defun glsl-operator (stream exp)
  (setf stream (or stream *standard-output*))
  (let ((op (car exp)))
    (case (length (cdr exp))
      (0
       (with-cl-io-syntax
         (error "No argument ~S" exp)))
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
    (funcall (formatter "~:/fude-gl:glsl-symbol/~<~@{[~W]~^~@_~}~:>") stream
             (cadr exp) (cddr exp))))

(defun glsl-let (stream exp)
  (setf stream (or stream *standard-output*))
  (labels ((rec (binds)
             (if (endp binds)
                 (progn
                  (funcall
                    (formatter
                     "~<~@{~W~^ ~:@/fude-gl:glsl-symbol/~^ = ~W;~:@_~}~:>~{~W~^ ~_~}")
                    stream
                    (loop :for bind :in (cadr exp)
                          :do (unless (= 3 (length bind))
                                (with-cl-io-syntax
                                  (error
                                    "Syntax error in LET: wrong binding form. ~:@_Require (var glsl-type initform) ~:@_~S"
                                    bind)))
                              (with-cl-io-syntax
                                (check-type (cadr bind) glsl-type))
                          :collect (cadr bind)
                          :collect (car bind)
                          :collect (caddr bind))
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
     (format output "Unknown slot named ~S for type ~S. ~:@_"
             (cell-error-name this) (slot-type this))
     (did-you-mean output (symbol-name (cell-error-name this))
                   (known-vars this)))))

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
    (with-cl-io-syntax
      (error "CONSTANT ~S needs type DECLAIMed." (second exp))))
  (funcall (formatter "const ~A ~A = ~A;~%") stream
           (symbol-camel-case (gethash (second exp) *declaims*))
           (symbol-camel-case (second exp)) (third exp)))

(defun glsl-defun (stream exp)
  (setf stream (or stream *standard-output*))
  (let* ((ftype (gethash (second exp) *declaims*))
         (*environment*
          (progn
           (push
            (list :lisp-name (symbol-name (second exp))
                  :name (symbol-camel-case (second exp))
                  :return (third ftype)
                  :args (mapcar
                          (lambda (var type)
                            (list (symbol-camel-case var)
                                  (symbol-camel-case type)))
                          (third exp) (second ftype)))
            (environment-function *environment*))
           (argument-environment *environment*
                                 :variable (var-info :local (mapcar #'list
                                                                    (third exp)
                                                                    (cadr
                                                                      ftype)))))))
    (unless ftype
      (with-cl-io-syntax
        (error "DEFUN ~S needs ftype DECLAIMed." (second exp))))
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

(defun glsl-struct-definition (stream structure-name &rest noise)
  (declare (ignore noise))
  (setf stream (or stream *standard-output*))
  (pprint-logical-block (stream nil)
    (format stream "struct ~A~%{~2I~:@_"
            (change-case:pascal-case (symbol-name structure-name)))
    (loop :for (slot . rest)
               :on (c2mop:class-slots
                     (c2mop:ensure-finalized (find-class structure-name)))
          :do (format stream "~/fude-gl:glsl-symbol/ ~/fude-gl:glsl-symbol/;"
                      (glsl-type slot) (c2mop:slot-definition-name slot))
          :if rest
            :do (pprint-newline :mandatory stream)
          :else
            :do (format stream "~I~:@_};~%"))))

(defun slot-exist-p (structure slot-name)
  (loop :for slot
             :in (c2mop:class-slots
                   (c2mop:ensure-finalized (find-class structure)))
        :thereis (eq slot-name (c2mop:slot-definition-name slot))))

(define-condition unknown-glsl-structure (fude-gl-error cell-error)
  ()
  (:report
   (lambda (this out)
     (format out "Unknown GLSL structure ~S." (cell-error-name this)))))

(define-condition missing-slot (fude-gl-error cell-error)
  ((structure :initarg :structure :reader glsl-structure))
  (:report
   (lambda (this out)
     (format out "Missing slot ~S in ~S. ~:@_" (cell-error-name this)
             (glsl-structure this))
     (did-you-mean out (symbol-name (cell-error-name this))
                   (mapcar #'c2mop:slot-definition-name
                           (c2mop:class-slots
                             (c2mop:ensure-finalized
                               (find-class (glsl-structure this))))))
     (format out " ~:@_To see all supported slots for ~S, evaluate ~S."
             (glsl-structure this)
             `(c2mop:class-slots
                (c2mop:ensure-finalized
                  (find-class ',(glsl-structure this))))))))

(defun glsl-slot-value (stream exp &rest noise)
  (declare (ignore noise))
  (with-cl-io-syntax
    (unless (= 2 (length (cdr exp)))
      (error 'glsl-argument-mismatch :form exp))
    (unless (glsl-structure-name-p (cadr exp))
      (error 'unknown-glsl-structure :name (cadr exp)))
    (unless (apply #'slot-exist-p (cdr exp))
      (with-hint (("Print all supported slots."
                   (c2mop:class-slots
                     (c2mop:ensure-finalized (find-class (cadr exp))))))
        (error 'missing-slot :name (caddr exp) :structure (cadr exp)))))
  (format stream "~A.~A" (symbol-camel-case (cadr exp))
          (symbol-camel-case (caddr exp))))

(defun glsl-dispatch ()
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil)))
    (set-pprint-dispatch 'symbol 'glsl-symbol)
    (set-pprint-dispatch '(cons (member setf)) 'glsl-setf)
    (set-pprint-dispatch '(cons (member aref)) 'glsl-aref)
    (set-pprint-dispatch '(cons (member * + / - <)) 'glsl-operator)
    (set-pprint-dispatch '(cons (member let)) 'glsl-let)
    (set-pprint-dispatch '(cons symbol) 'glsl-funcall -1)
    (set-pprint-dispatch '(cons (member return)) 'glsl-return)
    (set-pprint-dispatch '(cons (member with-slots)) 'glsl-with-slots)
    (set-pprint-dispatch '(cons (member if)) 'glsl-if)
    (set-pprint-dispatch '(cons (member define-symbol-macro))
                         'glsl-define-symbol-macro)
    (set-pprint-dispatch '(cons (member declaim)) 'glsl-declaim)
    (set-pprint-dispatch '(cons (member defconstant)) 'glsl-defconstant)
    (set-pprint-dispatch '(cons (member defun)) 'glsl-defun)
    (set-pprint-dispatch '(cons (member slot-value)) 'glsl-slot-value)
    *print-pprint-dispatch*))

(defun glsl-special-operator-p (symbol)
  #.(or #+sbcl
        `(loop :for entry :being :each :hash-value :of
                    (sb-pretty::pp-dispatch-cons-entries (glsl-dispatch))
               :for type = (sb-pretty::pprint-dispatch-entry-type entry)
               :when (and (typep type
                                 '(cons (eql cons) (cons (cons (eql member)))))
                          (find symbol (cdr (second type)))
                          (symbolp
                            (sb-pretty::pprint-dispatch-entry-fun entry))
                          (eq (find-package :fude-gl)
                              (symbol-package
                                (sb-pretty::pprint-dispatch-entry-fun entry))))
                 :return t)
        (warn "Not implemented for ~S." (lisp-implementation-type))))

(defun print-glsl (exp &optional stream)
  (let ((*print-pretty* t) (*print-pprint-dispatch* (glsl-dispatch)))
    (pprint exp stream)))

(defun pprint-glsl (stream exp &rest noise)
  (declare (ignore noise))
  (print-glsl exp stream))