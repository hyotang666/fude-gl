(in-package :fude-gl)

(declaim (optimize speed))

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