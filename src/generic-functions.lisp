(in-package :fude-gl)

(declaim (optimize speed))

;;;; CONDITION

(define-condition missing-shader (fude-gl-error cell-error)
  ((interface :initarg :interface :reader interface))
  (:report
   (lambda (this output)
     (format output "Missing shader named ~S. ~:@_" (cell-error-name this))
     (did-you-mean output (princ-to-string (cell-error-name this))
                   (loop :for method
                              :in (c2mop:generic-function-methods
                                    (interface this))
                         :for specializer
                              := (car (c2mop:method-specializers method))
                         :when (typep specializer 'c2mop:eql-specializer)
                           :collect (c2mop:eql-specializer-object
                                      specializer)))
     (format output " ~:@_To see all known shaders, evaluate ~S."
             `(c2mop:generic-function-methods
                #',(c2mop:generic-function-name (interface this)))))))

;;;; VERTEX-SHADER

(defgeneric vertex-shader (name)
  (:documentation "Return vertex shader code string."))

(defmethod no-applicable-method ((gf (eql #'vertex-shader)) &rest args)
  (error 'missing-shader :name (car args) :interface gf))

;;;; FRAGMENT-SHADER

(defgeneric fragment-shader (name)
  (:documentation "Return fragment shader code string."))

(defgeneric (setf fragment-shader) (new name)
  (:method ((new string) (name symbol))
    (let* ((gf (c2mop:ensure-generic-function 'fragment-shader))
           (method
            (find-method gf nil (list (c2mop:intern-eql-specializer name)))))
      (multiple-value-bind (lambda initargs)
          (c2mop:make-method-lambda gf method
                                    `(lambda ,(c2mop:method-lambda-list method)
                                       ,new)
                                    nil)
        (add-method gf
                    (apply #'make-instance
                           (c2mop:generic-function-method-class gf)
                           :lambda-list (c2mop:method-lambda-list method)
                           :specializers (c2mop:method-specializers method)
                           :function (compile nil lambda) initargs))))
    new)
  (:documentation "For debug use."))

(defmethod no-applicable-method ((gf (eql #'fragment-shader)) &rest args)
  (error 'missing-shader :name (car args) :interface gf))

;;;; UNIFORMS

(defgeneric uniforms (name)
  (:documentation "Return associated uniform name strings."))

(defmethod no-applicable-method ((gf (eql #'uniforms)) &rest args)
  (error 'missing-shader :name (car args) :interface gf))

;;;; CONSTRUCT

(defgeneric construct (thing)
  (:documentation "Requesting openGL to construct objects."))

;;;; DESTRUCT

(defgeneric destruct (thing)
  ;; NOTE!
  ;; When code fails while constructing,
  ;; DESTRUCT may be called before bound slot.
  (:documentation "Requesting openGL to destruct objects."))

;;;; CREATE-VERTEX-ARRAY

(defgeneric create-vertex-array (vertices))

;;;; DRAW

(defgeneric draw (thing) (:documentation "Request openGL to draw THING."))

;;;; SEND

(defgeneric send (object to &key))

;;;; CONSTRUCTED-P

(defgeneric constructed-p (vertices)
  (:documentation "Is VERTICES constructed in GL?"))