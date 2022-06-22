(in-package :fude-gl)

;;;; GLSL-STRUCTURE

(defclass glsl-structure-class (standard-class) ()
  (:documentation "Meta class for glsl structure."))

(defmethod c2mop:validate-superclass
           ((s glsl-structure-class) (c standard-class))
  t)

(deftype glsl-type ()
  '(member :bool :int :uint :float :vec2 :vec3 :vec4 :uvec3 :mat4 :|sampler2D|))

(defclass glsl-slot-mixin ()
  ((glsl-type :type glsl-type :initarg :glsl-type :reader glsl-type))
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

(defun validate-slot-specs (slot-specs)
  (loop :for slot-spec :in slot-specs
        :with sentinel := '#:sentinel
        :for glsl-type := (getf (cdr slot-spec) :glsl-type sentinel)
        :when (eq sentinel glsl-type)
          :do (error ":GLSL-TYPE is required. ~S" slot-spec)
        :unless (typep glsl-type 'glsl-type)
          :do (cerror "Anyway use it." "Unknown GLSL-TYPE. ~S" glsl-type)))

(defmacro define-glsl-structure (name () (&rest slot-spec*) &body option*)
  ;; Trivial-error-check.
  (validate-slot-specs slot-spec*)
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

(eprot:defenv :fude-gl
  :use :glsl
  :handler
  ((location (symbol fixnum) (decl-form env) (declare (ignore env))
    "Specify location of the attribute SYMBOL."
    (values :variable
            (eprot:decl-spec-bind (decl-name var location)
                decl-form
              (list (list var decl-name location)))))
   (attribute (symbol keyword) (decl-form env) (declare (ignore env))
    "Specify SYMBOL as fude-gl ATTIBUTE."
    (values :bind
            (eprot:decl-spec-bind (decl-name var attribute)
                decl-form
              (list (list var decl-name attribute)))))
   (constant (t &rest symbol) (decl-form env) "Mark SYMBOL as a constant."
    (declare (ignore env))
    (eprot:decl-spec-bind (decl-name value &rest names)
        decl-form
      (values :variable
              (mapcar (lambda (name) (list name decl-name value)) names))))
   (refered (symbol) (decl-form env) "Mark the SYMBOL is refered."
    (eprot:decl-spec-bind (decl-name var-name)
        decl-form
      (declare (ignore decl-name))
      (let ((info (nth-value 2 (eprot:variable-information var-name env))))
        (cond
          ((null info) (uiop:style-warn "Unknown var to refer. ~S" var-name))
          ((assoc 'ignorable info) nil)
          ((assoc 'ignore info)
           (uiop:style-warn "Use ~S but it is declared as ignore." var-name))
          ((find-symbol (symbol-name var-name) :glsl-symbols.variables) nil)
          (t
           (setf (eprot:variable-information var-name env)
                   `(ignorable ,var-name))))))
    nil)))

(defun variable-information (symbol &optional env)
  (let ((glsl-symbol
         (find-symbol (symbol-name symbol) :glsl-symbols.variables)))
    (eprot:variable-information (or glsl-symbol symbol) env)))

(defun slot-truename (spec)
  (etypecase spec (symbol spec) ((cons symbol (cons symbol null)) (cadr spec))))

(defun list-all-known-vars ()
  (apply #'append
         (uiop:while-collecting (acc)
           (eprot::do-env (e eprot:*environment*)
             (acc (eprot::environment-variable e))))))

(define-condition unused-variable (style-warning)
  ((name :initarg :name :reader unused-var)
   (context :initarg :context :reader context))
  (:report
   (lambda (this output)
     (format output "Variable ~S is not used. ~:@_The context is ~S."
             (unused-var this) (context this)))))

(defvar *cl-pp-dispatch* (copy-pprint-dispatch nil))

(defmacro with-cl-io-syntax (&body body)
  ;; WARN, CERROR, BREAK needs this macro.
  ;; Otherwise GLSL-DISPATCH table is used by debugger.
  `(let ((*print-pprint-dispatch* *cl-pp-dispatch*))
     ,@body))

(defun check-ref (vars)
  (dolist (var vars)
    (let ((info (nth-value 2 (variable-information var eprot:*environment*))))
      (unless (or (assoc 'ignore info) (assoc 'ignorable info))
        (with-cl-io-syntax
          (warn 'unused-variable
                :name var
                :context (eprot:context eprot:*environment*)))))))

(defun function-information (symbol &optional env)
  (let ((glsl-symbol
         (or (find-symbol (symbol-name symbol) :glsl-symbols.functions)
             (find-symbol (symbol-name symbol) :glsl-symbols.operators)
             (find-symbol (symbol-name symbol) :glsl-symbols.types))))
    (eprot:function-information (or glsl-symbol symbol) env)))

(defun list-all-known-functions ()
  (apply #'append
         (uiop:while-collecting (acc)
           (eprot::do-env (e eprot:*environment*)
             (acc (eprot::environment-function e))))))

;;;; GLSL PRINTERS

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

(defvar *var-check-p*
  nil
  "Boolean to specify to check variable existence.
This is useful especially for enabling with ~W format control.")

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
  (multiple-value-bind (var-type lexicalp infos)
      (variable-information exp eprot:*environment*)
    (declare (ignore var-type))
    (cond
      (lexicalp
       (unless not-ref-p ; means refered-p
         (eprot:proclaim `(refered ,exp)))
       (write-string (cdr (assoc 'glsl-env:notation infos)) stream))
      (infos ; global one.
       (write-string (cdr (assoc 'glsl-env:notation infos)) stream))
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
     (let ((info
            (nth-value 2
                       (when (typep (form this)
                                    '(cons symbol (cons symbol null)))
                         (variable-information (cadr (form this))
                                               eprot:*environment*)))))
       (when (and info (glsl-structure-name-p (cadr (assoc 'type info))))
         (format output
                 " ~:@_Or slot reader may missing. To see all supported slot readers, evaluate ~S."
                 `(class-readers ',(getf info :type))))))))

(define-condition glsl-argument-mismatch (fude-gl-error)
  ((form :initarg :form :reader form))
  (:report
   (lambda (this out)
     (format out "Arguments for ~S does not match its signature. ~S~@[~?~]"
             (car (form this)) (form this)
             (multiple-value-bind (fun-type lexicalp infos)
                 (function-information (car (form this)) eprot:*environment*)
               (declare (ignore fun-type))
               (when (and (null lexicalp) infos) ; global glsl function.
                 " ~:@_For detail, evaluate ~S."))
             (list
               `(function-information ',(car (form this))
                                      eprot:*environment*))))))

(defun glsl-swizzling (stream exp)
  (setf stream (or stream *standard-output*))
  (unless (= 1 (length (cdr exp)))
    (error 'glsl-argument-mismatch :form exp))
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

(defun form-type-notation (exp)
  (etypecase exp
    (list ; function call
     (let ((info
            (nth-value 2 (function-information (car exp) eprot:*environment*))))
       (if (null info)
           (error "Could not detect form type of ~S." exp)
           (let ((return-type (third (cdr (assoc 'ftype info)))))
             (if (not return-type)
                 (error "Missing function return type. ~S" info)
                 (etypecase return-type
                   (keyword ; type constructor.
                    (or (cdr
                          (assoc 'glsl-env:notation
                                 (nth-value 2
                                            (function-information return-type
                                                                  eprot:*environment*))))
                        (error "Missing type constructor infor. ~S"
                               return-type)))
                   (symbol ; glsl-structure which should have delcaration as type.
                    (form-type-notation return-type))))))))
    (symbol ; var reference
     (let ((info (nth-value 2 (variable-information exp eprot:*environment*))))
       (if (null info)
           (error "Could not detect type of ~S. Unknown var?" exp)
           (or (cdr (assoc 'glsl-env:notation info))
               (error "Missing variable type. ~S" info)))))))

(defun slot-notation (exp fun-info)
  ;; FIXME: Should we use another better declaration?
  (let ((structure-notation (form-type-notation (cadr exp))))
    (some
      (lambda (info)
        (and (eq 'glsl-env:notation (car info))
             (uiop:string-prefix-p structure-notation (cdr info))
             (cdr info)))
      fun-info)))

(defun glsl-funcall (stream exp)
  (setf stream (or stream *standard-output*))
  (if (gethash (symbol-name (car exp)) +swizzling+)
      (glsl-swizzling stream exp)
      (let ((info
             (nth-value 2
                        (function-information (car exp) eprot:*environment*))))
        #++
        (with-cl-io-syntax
          (break))
        ;; Is function known one?
        (unless info
          (with-cl-io-syntax
            (with-hint (("Print all known functions."
                         (list-all-known-functions)))
              (cerror "Anyway, print it." 'unknown-glsl-function
                      :name (car exp)
                      :form exp))))
        ;; Is it slot-reader?
        (if (eq :slot-reader (cdr (assoc 'attribute info)))
            ;; Does match arg length?
            (if (= 1 (length (cdr exp)))
                (write-string (slot-notation exp info) stream)
                (with-hint (("Print function informations." info))
                  (error 'glsl-argument-mismatch :form exp)))
            ;; Does match arg length?
            (if (or (null info) ; continued.
                    (and info
                         (loop :with actual = (length (cdr exp))
                               :for (decl-name . fun-specifier) :in info
                               :thereis (and (eq 'ftype decl-name)
                                             (= actual
                                                (length
                                                  (cadr fun-specifier)))))))
                (funcall
                  (formatter "~/fude-gl:glsl-symbol/~:<~@{~W~^, ~@_~}~:>")
                  stream (car exp) (cdr exp))
                (with-hint (("Print function signatures."
                             (print
                               (remove-if-not
                                 (lambda (spec) (eq 'ftype (car spec))) info))))
                  (error 'glsl-argument-mismatch :form exp)))))))

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
    (funcall (formatter "~:/fude-gl:glsl-symbol/~<~@{[~W]~^~@_~}~:>") stream
             (cadr exp) (cddr exp))))

(defun glsl-let (stream exp)
  (setf stream (or stream *standard-output*))
  (labels ((rec (binds)
             (if (endp binds)
                 (let ((*var-check-p* t))
                   (funcall
                     (formatter
                      #.(concatenate 'string
                                     ;; type name, without var checking.
                                     "~<~@{~/fude-gl:glsl-symbol/"
                                     ;; SETFable place, with var checking.
                                     "~^ ~:@/fude-gl:glsl-symbol/"
                                     ;; Initform, with var checking.
                                     "~^ = ~W;~:@_~}~:>"
                                     ;; The body, with var checking.
                                     "~{~W~^ ~_~}"))
                     stream
                     (loop :for bind :in (cadr exp)
                           :do (unless (= 3 (length bind))
                                 (error
                                   "Syntax error in LET: wrong binding form. ~:@_Require (var glsl-type initform) ~:@_~S"
                                   bind))
                               (unless (typep (cadr bind) 'glsl-type)
                                 (error "Unknown glsl type. ~S" (cadr bind)))
                           :collect (cadr bind)
                           :collect (car bind)
                           :collect (caddr bind))
                     (cddr exp))
                   (when (every #'listp (cddr exp))
                     (check-ref (mapcar #'car (cadr exp)))))
                 (let ((eprot:*environment*
                        (eprot:augment-environment eprot:*environment*
                                                   :name (cons 'let
                                                               (caar binds))
                                                   :variable (list
                                                               (caar binds))
                                                   :declare (list
                                                              `(type
                                                                 ,(cadar binds)
                                                                 ,(caar binds))
                                                              `(glsl-env:notation
                                                                 ,(caar binds)
                                                                 ,(symbol-camel-case
                                                                    (caar
                                                                      binds)))))))
                   (rec (cdr binds))))))
    (rec (cadr exp))))

(defun glsl-return (stream exp)
  (setf stream (or stream *standard-output*))
  (unless (= 2 (length exp))
    (error 'glsl-argument-mismatch :form exp))
  (apply (formatter "~/fude-gl:glsl-symbol/ ~W;") stream exp))

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

#++
(defun glsl-with-slots (stream exp)
  (setf stream (or stream *standard-output*))
  (destructuring-bind
      (slots var &body body)
      (cdr exp)
    (let ((info (variable-information var *environment*)) class)
      ;; Does variable exist?
      (unless info
        (error 'unknown-variable :name var :known-vars (list-all-known-vars)))
      ;; Does glsl-structure exist?
      (unless (setq class (find-class (variable-information-type info) nil))
        (error 'unknown-glsl-structure :name (variable-information-type info)))
      ;; Does each slot exist?
      (let ((slot-defs (c2mop:class-slots (c2mop:ensure-finalized class))))
        (dolist (slot slots)
          (unless (find (slot-truename slot) slot-defs
                        :key #'c2mop:slot-definition-name)
            (error 'unknown-slot
                   :name (slot-truename slot)
                   :type var
                   :known-vars (mapcar #'c2mop:slot-definition-name
                                       slot-defs)))))
      (let ((*environment*
             (argument-environment *environment* :variable
              (var-info :slot slots :structure var :var-name
               (variable-information-name info)))))
        ;; The body.
        (dolist (exp body) (write exp :stream stream))))))

(defun glsl-if (stream exp)
  (setf stream (or stream *standard-output*))
  (when (find-if (lambda (exp) (and (listp exp) (find (car exp) '(let))))
                 (cddr exp))
    (error "In valid expression for IF. Use COND instead. ~S" exp))
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
  (loop :for decl-form :in (cdr exp)
        :do (eprot:proclaim decl-form)))

(defun glsl-defconstant (stream exp)
  (setf stream (or stream *standard-output*))
  (unless (gethash (second exp) *declaims*)
    (error "CONSTANT ~S needs type DECLAIMed." (second exp)))
  (funcall (formatter "const ~A ~A = ~A;~%") stream
           (symbol-camel-case (gethash (second exp) *declaims*))
           (symbol-camel-case (second exp)) (third exp)))

(defun struct-readers (struct-names)
  (delete-duplicates
    (uiop:while-collecting (acc)
      (dolist (name struct-names)
        (dolist
            (slot
             (c2mop:class-direct-slots
               (c2mop:ensure-finalized (find-class name))))
          (dolist (reader (c2mop:slot-definition-readers slot))
            (acc reader)))))))

(defun glsl-defun (stream exp)
  (setf stream (or stream *standard-output*))
  (destructuring-bind
      (name lambda-list &body body)
      (cdr exp)
    (let ((info
           (or (nth-value 2
                          (eprot:function-information name
                                                      eprot:*environment*))
               (error "DEFUN ~S needs ftype DECLAIMed." name))))
      (destructuring-bind
          (arg-types return)
          (cddr (assoc 'ftype info))
        (let* ((functions
                (cons name
                      (struct-readers
                        (remove-if-not #'glsl-structure-name-p arg-types))))
               (decls
                `((ftype (function
                          ,(mapcar
                             (lambda (var type)
                               (list (symbol-camel-case var)
                                     (symbol-camel-case type)))
                             lambda-list arg-types)
                          ,return)
                         ,name)
                  (glsl-env:notation ,name ,(symbol-camel-case name))
                  ,@(mapcan
                      (lambda (var type)
                        `((type ,type ,var)
                          (glsl-env:notation ,var ,(symbol-camel-case var))))
                      lambda-list arg-types)))
               (eprot:*environment*
                (eprot:augment-environment eprot:*environment*
                                           :name name
                                           :function functions
                                           :variable lambda-list
                                           :declare decls)))
          (funcall
            (formatter
             #.(apply #'concatenate 'string
                      (alexandria:flatten
                        (list "~(~A~)~^ ~@_" ; return type
                              "~A~^ ~@_" ; function name.
                              (list "~:<" ; logical block for args.
                                    "~@{~A~^ ~A~^, ~}" ; argbody.
                                    "~:>~^ ~%")
                              "~:<{~;~3I~:@_" ; function body.
                              "~@{~A~^ ~_~}~%" "~;}~:>~%"))))
            stream
            (if (equal '(values) return)
                :void
                return)
            name
            (loop :for type :in arg-types
                  :for name :in lambda-list
                  :collect (if (glsl-structure-name-p type)
                               (change-case:pascal-case (symbol-name type))
                               (symbol-camel-case type))
                  :collect (symbol-camel-case name))
            body)
          (when (every #'listp body)
            (check-ref lambda-list)))))))

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
  (unless (= 2 (length (cdr exp)))
    (error 'glsl-argument-mismatch :form exp))
  (unless (glsl-structure-name-p (cadr exp))
    (error 'unknown-glsl-structure :name (cadr exp)))
  (unless (apply #'slot-exist-p (cdr exp))
    (with-hint (("Print all supported slots."
                 (c2mop:class-slots
                   (c2mop:ensure-finalized (find-class (cadr exp))))))
      (error 'missing-slot :name (caddr exp) :structure (cadr exp))))
  (format stream "~A.~A" (symbol-camel-case (cadr exp))
          (symbol-camel-case (caddr exp))))

(defun glsl-cond (out exp &rest noise)
  (declare (ignore noise))
  (pprint-logical-block (out nil)
    (write-string "if" out)
    (loop :with body = (formatter " ~:@_{~4I~:@_~@{~W~^ ~:@_~} ~I~:@_} ~:@_")
          :for (clause . rest) :on (cdr exp)
          :do (funcall (formatter "(~W)") out (car clause))
              (apply body out (cdr clause))
          :if rest
            :if (eq t (caar rest))
              :do (funcall (formatter "else") out)
                  (apply body out (cdar rest))
                  (loop-finish)
            :else
              :do (funcall (formatter "else if") out)
          :else
            :do (loop-finish))))

(defun glsl-dotimes (out exp &rest noise)
  (declare (ignore noise))
  (destructuring-bind
      ((var times) &body body)
      (cdr exp)
    (let ((eprot:*environment*
           (eprot:augment-environment eprot:*environment*
                                      :variable (list var)
                                      :declare `((type :int ,var)
                                                 (glsl-env:notation ,var
                                                  ,(symbol-camel-case var))))))
      (pprint-logical-block (out nil)
        ;; Var def.
        (funcall (formatter "for (int ~/fude-gl:glsl-symbol/") out var)
        ;; Terminal test.
        (funcall
          (formatter
           " = 0; ~/fude-gl:glsl-symbol/ < ~:@/fude-gl:glsl-symbol/;")
          out var times)
        ;; Update.
        (funcall (formatter " ~/fude-gl:glsl-symbol/++){ ~4I~:@_") out var)
        ;; The body.
        (funcall (formatter "~{~W;~^ ~:@_~}") out body)
        (funcall (formatter "~I~:@_}") out)))))

(defun glsl-incf (out exp &rest noise)
  (declare (ignore noise))
  (destructuring-bind
      (place &optional num)
      (cdr exp)
    (if num
        (funcall (formatter "~W += ~W") out place num)
        (funcall (formatter "~W++") out place))))

(defun glsl-constant-definition (out exp &rest noise)
  (declare (ignore noise))
  (funcall (formatter "#define ~A ~A") out
           (change-case:constant-case (symbol-name (cadr exp))) (caddr exp)))

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
    (set-pprint-dispatch '(cons (member cond)) 'glsl-cond)
    (set-pprint-dispatch '(cons (member dotimes)) 'glsl-dotimes)
    (set-pprint-dispatch '(cons (member incf)) 'glsl-incf)
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
    (handler-bind ((error
                     (lambda (condition)
                       (if (find-restart 'continue condition)
                           ;; Require *print-pprint-dispatch* equals *cl-pp-dispatch*.
                           ;; Otherwise debugger can not print correctly.
                           (unless (eq *print-pprint-dispatch*
                                       *cl-pp-dispatch*)
                             (setq *print-pprint-dispatch* *cl-pp-dispatch*)
                             (warn
                               "Reset *print-pprint-dispatch* for debugger, use ~S."
                               'with-cl-io-syntax))
                           ;; Give up. Reset *print-pprint-dispatch* for correct cl printing.
                           (setq *print-pprint-dispatch* *cl-pp-dispatch*))))
                   (warning
                    (lambda (condition)
                      (unless (eq *print-pprint-dispatch* *cl-pp-dispatch*)
                        (setq *print-pprint-dispatch* *cl-pp-dispatch*)
                        (error
                          "Could not print warning correctly due to not cl-syntax. ~:@_Use ~S. ~S"
                          'with-cl-io-syntax condition)))))
      (pprint exp stream))))

(defun pprint-glsl (stream exp &rest noise)
  (declare (ignore noise))
  (print-glsl exp stream))