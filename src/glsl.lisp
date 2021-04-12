(in-package :fude-gl)

(defun symbol-camel-case (s) (change-case:camel-case (symbol-name s)))

(defvar *alias* nil)

(deftype glsl-type () '(member :float :vec2 :vec3 :vec4 :mat4 :|sampler2D|))

(defun glsl-symbol (stream exp &rest noise)
  (declare (ignore noise))
  (let ((alias (assoc exp *alias*)))
    (if alias
        (glsl-symbol stream (cdr alias))
        (cond
          ((uiop:string-prefix-p "GL-" exp)
           (format stream "gl_~A"
                   (change-case:pascal-case (subseq (symbol-name exp) 3))))
          ((find-if #'lower-case-p (symbol-name exp))
           (write-string (symbol-name exp) stream))
          (t
           (write-string (change-case:camel-case (symbol-name exp)) stream))))))

(defun glsl-setf (stream exp)
  (setf stream (or stream *standard-output*))
  (funcall (formatter "~{~/fude-gl::glsl-symbol/ = ~W;~:@_~}") stream
           (cdr exp)))

(defun glsl-funcall (stream exp)
  (setf stream (or stream *standard-output*))
  (funcall (formatter "~W~:<~@{~W~^, ~@_~}~:>") stream (car exp) (cdr exp)))

(defun glsl-operator (stream exp)
  (setf stream (or stream *standard-output*))
  (let ((op (car exp)))
    (case (length (cdr exp))
      (0 (error "No argument ~S" exp))
      (1 (format stream "~A~A" (symbol-name op) (cadr exp)))
      (2
       (format stream "(~W ~A ~W)" (second exp) (symbol-name op) (third exp)))
      (otherwise
       (loop :for (form . rest) :on (cdr exp)
             :initially (write-char #\( stream)
             :do (write form :stream stream)
                 (when rest
                   (format stream " ~A " (symbol-name op)))
             :finally (write-char #\) stream))))))

(defun glsl-aref (stream exp)
  (setf stream (or stream *standard-output*))
  (funcall (formatter "~W~:<[~;~@{~W~^, ~@_~}~;]~:>") stream (cadr exp)
           (cddr exp)))

(defun glsl-let (stream exp)
  (setf stream (or stream *standard-output*))
  (funcall (formatter "~<~@{~W~^ ~W~^ = ~W;~:@_~}~:>~{~W~^ ~_~}") stream
           (loop :for (name type init) :in (cadr exp)
                 :do (check-type type glsl-type)
                 :collect type
                 :collect name
                 :collect init)
           (cddr exp)))

(defun glsl-swizzling (stream exp)
  (setf stream (or stream *standard-output*))
  (format stream "~W.~W" (cadr exp) (car exp)))

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
  (let ((ftype (gethash (second exp) *declaims*)))
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
                                "~@{~{~(~A~)~^ ~A~^, ~}~}" ; argbody.
                                "~:>~^ ~%")
                          "~:<{~;~3I~:@_" ; function body.
                          "~@{~A~^ ~_~}~%" "~;}~:>~%"))))
        stream
        (if (equal '(values) return)
            :void
            return)
        (second exp) (mapcar #'list arg-types (third exp)) (cdddr exp)))))

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
  (let ((*print-pretty* t) (*print-pprint-dispatch* (glsl-dispatch)))
    (pprint exp stream)))

(defun pprint-glsl (stream exp &rest noise)
  (declare (ignore noise))
  (print-glsl exp stream))
