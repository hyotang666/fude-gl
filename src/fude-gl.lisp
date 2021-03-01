(in-package :cl-user)

(defpackage :fude-gl
  (:use :cl)
  (:export ;;;; MAIN API.
           #:defshader
           ;;;; FUNDAMENTAL-CLASSES
           #:vertex
           #:color
           #:coord
           ;;;; UTILITIES
           #:with-shader
           #:with-gl-array
           #:with-textures
           #:with-2d-textures
           #:foreign-type))

(in-package :fude-gl)

;;;; METACLASS

(defclass vector-class (standard-class) ())

(defmethod c2mop:validate-superclass ((c vector-class) (s standard-class)) t)

;;;; CLASSES

(defclass vertex ()
  ((x :initarg :x :type single-float) (y :initarg :y :type single-float))
  (:metaclass vector-class))

(defclass color ()
  ((r :initarg :r :type single-float)
   (g :initarg :g :type single-float)
   (b :initarg :b :type single-float))
  (:metaclass vector-class))

(defclass coord ()
  ((u :initarg :u :type single-float) (v :initarg :v :type single-float))
  (:metaclass vector-class))

;;;; CONSTRUCTOR

(defmethod make-instance :around ((c vector-class) &rest args)
  (let ((values
         (uiop:while-collecting (acc)
           (dolist (c (nreverse (class-list c)))
             (dolist (slot (c2mop:class-direct-slots c))
               (acc
                (or (getf args
                          (intern
                            (format nil "~A" (c2mop:slot-definition-name slot))
                            :keyword))
                    (error "~S is required." slot))))))))
    (make-array (length values)
                :initial-contents values
                :element-type 'single-float)))

;;;; GENERIC-FUNCTIONS

(defgeneric vertex-shader (name)
  (:documentation "Accept class name, return its vertex shader code string."))

(defgeneric fragment-shader (name)
  (:documentation "Accept class name, return its fragment shader code string."))

;;;; DSL
;;; DEFSHADER

(defmacro defshader (name version superclasses &body shader*)
  ;; Trivial syntax check.
  (check-type name symbol)
  (check-type version (or symbol integer))
  (assert (and (listp superclasses) (every #'find-class superclasses)))
  (assert (every (lambda (s) (find (car s) '(:vertex :fragment))) shader*))
  ;; binds
  (let ((format
         #.(concatenate 'string "#version ~A core~%" ; version
                        "~{in ~A ~A;~%~}~&" ; in
                        "~{out ~A ~A;~%~}~&" ; out
                        "~@[~{uniform ~A ~A;~%~}~]~&" ; uniforms
                        "void main () {~%~{~A~^~%~}~%}" ; the body.
                        )))
    (flet ((defs (list)
             (loop :for (name type) :in list
                   :collect (change-case:camel-case (symbol-name type))
                   :collect (symbol-name name))))
      ;; The body.
      `(progn
        (defclass ,name ,superclasses () (:metaclass vector-class))
        ,@(labels ((rec (shaders in acc)
                     (if (endp shaders)
                         (nreverse acc)
                         (body (car shaders) (cdr shaders) in acc)))
                   (body (shader rest in acc)
                     (destructuring-bind
                         (type out &rest main)
                         shader
                       (let* ((&uniform
                               (position-if
                                 (lambda (x)
                                   (and (symbolp x) (string= '&uniform x)))
                                 out))
                              (vars (and out (defs (subseq out 0 &uniform)))))
                         (rec rest `',vars
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
                                          `(format nil (formatter ,format)
                                                   ',version ,in ',vars
                                                   ',(and &uniform
                                                          (defs
                                                            (subseq out
                                                                    (1+
                                                                      &uniform))))
                                                   ',main))))
                                acc))))))
            (rec shader*
                 `(loop :for c :in (class-list (find-class type))
                        :for slots = (c2mop:class-direct-slots c)
                        :when slots
                          :collect (format nil "vec~D" (length slots))
                          :and :collect (change-case:camel-case
                                          (symbol-name (class-name c))))
                 nil))))))

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
                                        "~@{~W~^ ~@_~}" ; out lambda var.
                                        "~:>~^ ~_")
                                  "~@{~W~^ ~_~}" ; clause body.
                                  "~:>~^ ~_")
                            "~}")
                      "~:>"))))
    stream exp))

(set-pprint-dispatch '(cons (member defshader)) 'pprint-defshader)

;;;; UTILITIES
;;; WITH-GL-ARRAY

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

(defun make-gl-array (initial-contents &key (element-type :float))
  (let* ((length (length initial-contents))
         (a (gl:alloc-gl-array element-type length)))
    (dotimes (i length a) (setf (gl:glaref a i) (aref initial-contents i)))))

(defmacro with-gl-array ((&rest bind*) &body body)
  `(let ,(mapcar
           (lambda (bind)
             (check-type bind (cons symbol (cons t null)))
             (destructuring-bind
                 (var vector)
                 bind
               (let ((v (gensym "VECTOR")) (type (gensym "TYPE")))
                 `(,var
                   (let* ((,v ,vector) (,type (array-element-type ,v)))
                     (make-gl-array ,v
                                    :element-type (foreign-type ,type
                                                                :cffi t)))))))
           bind*)
     (unwind-protect (progn ,@body)
       ,@(mapcar (lambda (bind) `(gl:free-gl-array ,(car bind))) bind*))))

;;; WITH-BUFFER

(deftype buffer-usage () '(member :static-draw :stream-draw :dynamic-draw))

(defmacro with-buffer ((&rest bind*) &body body)
  `(destructuring-bind
       ,(mapcar #'car bind*)
       (gl:gen-buffers ,(length bind*))
     (unwind-protect
         (progn
          ,@(mapcan
              (lambda (bind)
                (destructuring-bind
                    (var array
                     &key (target :array-buffer) (usage :static-draw))
                    bind
                  `((gl:bind-buffer ,target ,var)
                    (gl:buffer-data ,target (the buffer-usage ,usage)
                                    ,array))))
              bind*)
          ,@body)
       (gl:delete-buffers (list ,@(mapcar #'car bind*))))))

;;; WITH-PROG

(defmacro with-prog ((var vertex-shader fragment-shader) &body body)
  (let ((vs (gensym "VERTEX-SHADER")) (fs (gensym "FRAGMENT-SHADER")))
    `(let ((,var (gl:create-program)))
       (labels ((s-compile (id source)
                  (gl:shader-source id source)
                  (gl:compile-shader id)
                  (may-warn (gl:get-shader-info-log id))
                  (gl:attach-shader ,var id))
                (may-warn (log)
                  (unless (equal "" log)
                    (warn log))))
         (unwind-protect
             (let ((,vs (gl:create-shader :vertex-shader))
                   (,fs (gl:create-shader :fragment-shader)))
               (unwind-protect
                   (progn
                    (s-compile ,vs ,vertex-shader)
                    (s-compile ,fs ,fragment-shader)
                    (gl:link-program ,var)
                    (may-warn (gl:get-program-info-log ,var))
                    (gl:use-program ,var))
                 (gl:delete-shader ,fs)
                 (gl:delete-shader ,vs))
               ,@body)
           (gl:delete-program ,var))))))

;;; LINK-ATTRIBUTES

(defun class-list (class)
  "Return class list specified to abstract oder, superclasses are reverse order."
  (uiop:while-collecting (acc)
    (labels ((rec (c)
               (unless (eq 'standard-object (class-name c))
                 (acc c)
                 (mapc #'rec (reverse (c2mop:class-direct-superclasses c))))))
      (rec class))))

(defun link-attributes (class program)
  (labels ((rec (class-list total-length funs)
             (if (endp class-list)
                 (let ((total (apply #'+ total-length)))
                   (loop :for f :in funs
                         :for l :in total-length
                         :do (funcall f total l offset)
                         :sum l :into offset))
                 (let ((slots
                        (length (c2mop:class-direct-slots (car class-list)))))
                   (if (zerop slots)
                       (rec (cdr class-list) total-length funs)
                       (rec (cdr class-list)
                            (cons (the (integer 1 4) slots) total-length)
                            (cons (processer (car class-list)) funs))))))
           (processer (class)
             (lambda (total-length length offset)
               (let* ((location
                       (gl:get-attrib-location program
                                               (change-case:camel-case
                                                 (symbol-name
                                                   (class-name class)))))
                      (slots
                       (c2mop:class-direct-slots
                         (c2mop:ensure-finalized class)))
                      (type
                       (ecase (c2mop:slot-definition-type (car slots))
                         (single-float :float)))
                      (size (cffi:foreign-type-size type)))
                 (when (minusp location)
                   (error "Variable ~S is not active in program ~S"
                          (change-case:camel-case
                            (symbol-name (class-name class)))
                          program))
                 (gl:vertex-attrib-pointer location length type nil ; As
                                                                    ; normalized-p
                                           (* total-length size)
                                           (* offset size))
                 (gl:enable-vertex-attrib-array location)))))
    (rec (class-list (find-class class)) nil nil)))

;;; WITH-VERTEX-ARRAY

(defmacro with-vertex-array ((&rest bind*) &body body)
  `(let ,(mapcar (lambda (bind) `(,(car bind) (gl:gen-vertex-array))) bind*)
     (unwind-protect
         (progn
          ,@(mapcan
              (lambda (bind)
                `((gl:bind-vertex-array ,(car bind)) ,@(cdr bind)))
              bind*)
          ,@body)
       (gl:delete-vertex-arrays (list ,@(mapcar #'car bind*))))))

;;; WITH-TEXTURES

(defmacro with-textures ((&rest bind*) &body body)
  `(destructuring-bind
       ,(mapcar #'car bind*)
       (gl:gen-textures ,(length bind*))
     (unwind-protect
         (progn
          ,@(let ((active -1))
              (mapcan
                (lambda (bind)
                  (destructuring-bind
                      (var form
                       &key (type :texture-2d) (min :linear) (mag :linear)
                       (wrap-s :repeat) (wrap-t :repeat))
                      bind
                    `((gl:active-texture ,(incf active))
                      (gl:bind-texture ,type ,var)
                      (gl:tex-parameter ,type :texture-min-filter ,min)
                      (gl:tex-parameter ,type :texture-mag-filter ,mag)
                      (gl:tex-parameter ,type :texture-wrap-s ,wrap-s)
                      (gl:tex-parameter ,type :texture-wrap-t ,wrap-t) ,form)))
                bind*))
          ,@body)
       (gl:delete-textures (list ,@(mapcar #'car bind*))))))

(defun pprint-with-textures (stream exp)
  (funcall
    (formatter
     #.(apply #'concatenate 'string
              (alexandria:flatten
                (list "~:<" ; pprint-logical-block
                      "~W~^ ~1I~@_" ; operator.
                      (list "~:<" ; binds
                            "~@{" ; iterate binds.
                            (list "~:<" ; each bind
                                  "~W~^ ~:I~:_" ; var
                                  "~@{~W~^ ~W~^ ~_~}" ; k-v options.
                                  "~:>~^ ~_")
                            "~}" ; end of iterate.
                            "~:>~^ ~:@_")
                      "~@{~W~^ ~_~}" ; the body.
                      "~:>"))))
    stream exp))

(set-pprint-dispatch '(cons (member with-textures)) 'pprint-with-textures)

;;; WITH-SHADER

(defmacro with-shader ((&rest binds) &body body)
  (let* ((length (length binds))
         (array-vars (alexandria:make-gensym-list length))
         (buf-vars (alexandria:make-gensym-list length)))
    `(with-gl-array ,(mapcar (lambda (b g) `(,g ,(cadr b))) binds array-vars)
       (with-buffer ,(mapcar #'list buf-vars array-vars)
         ,@(labels ((rec (binds)
                      (if (endp binds)
                          body
                          `((with-prog (,(caar binds)
                                        (vertex-shader ',(caar binds))
                                        (fragment-shader ',(caar binds)))
                              (with-vertex-array ((,(gensym)
                                                   (link-attributes
                                                     ',(caar binds)
                                                     ,(caar binds))))
                                ,@(rec (cdr binds))))))))
             (rec binds))))))

;;;; WITH-2D-TEXTURES

(defmacro with-2d-textures ((&rest binds) &body body)
  `(with-textures ,(mapcar
                     (lambda (bind)
                       (let ((a (gensym "ARRAY"))
                             (tc (gensym "TEXTURE-COMPONENTS")))
                         (destructuring-bind
                             (var array &rest options)
                             bind
                           `(,var
                             (let ((,a ,array))
                               (flet ((,tc (array)
                                        (ecase (array-dimension array 2)
                                          (3 :rgb)
                                          (4 :rgba))))
                                 (gl:tex-image-2d :texture-2d 0 ; mipmap depth
                                                  (,tc ,a)
                                                  (array-dimension ,a 0) ; width
                                                  (array-dimension ,a 1) ; height
                                                  0 ; legacy
                                                  (,tc ,a)
                                                  (foreign-type
                                                    (array-element-type ,a))
                                                  (make-array
                                                    (array-total-size ,a)
                                                    :element-type (array-element-type
                                                                    ,a)
                                                    :displaced-to ,a))))
                             ,@options))))
                     binds)
     ,@body))
