(in-package :cl-user)

(defpackage :fude-gl
  (:use :cl)
  (:export ;;;; MAIN API.
           #:defshader
           ;;;; FUNDAMENTAL-CLASSES
           #:xy
           #:st
           #:rgb
           ;;;; UTILITIES
           #:radians
           #:with-shader
           #:with-gl-vector
           #:with-textures
           #:with-2d-textures
           #:with-clear
           #:foreign-type))

(in-package :fude-gl)

(defun radians (degrees) (* degrees (/ pi 180)))

;;;; METACLASS

(defclass vector-class (standard-class) ())

(defmethod c2mop:validate-superclass ((c vector-class) (s standard-class)) t)

;;;; CLASSES
#| NOTE:
 | T is constant.
 | We could not name a slot with constant (at least in sbcl).
 | So we decide to use prefix % for every slot name.
 | We choice having single rule rather than having corner case.
 |#

(defclass xy ()
  ((%x :initarg :x :type single-float) (%y :initarg :y :type single-float))
  (:metaclass vector-class))

(defclass xyz ()
  ((%x :initarg :x :type single-float)
   (%y :initarg :y :type single-float)
   (%z :initarg :z :type single-float))
  (:metaclass vector-class))

(defclass st ()
  ((%s :initarg :s :type single-float) (%t :initarg :t :type single-float))
  (:metaclass vector-class))

(defclass rgb ()
  ((%r :initarg :r :type single-float)
   (%g :initarg :g :type single-float)
   (%b :initarg :b :type single-float))
  (:metaclass vector-class))

;;;; CONSTRUCTOR

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
    (labels ((defs (list)
               (loop :for (name type) :in list
                     :collect (change-case:camel-case (symbol-name type))
                     :collect (change-case:camel-case (symbol-name name))))
             (rec (shaders in acc)
               (if (endp shaders)
                   (nreverse acc)
                   (body (car shaders) (cdr shaders) in acc)))
             (body (shader rest in acc)
               (destructuring-bind
                   (type out &rest main)
                   shader
                 (let* ((&uniform
                         (position-if
                           (lambda (x) (and (symbolp x) (string= '&uniform x)))
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
                                    `(format nil (formatter ,format) ',version
                                             ,in ',vars
                                             ',(and &uniform
                                                    (defs
                                                      (subseq out
                                                              (1+ &uniform))))
                                             ',main))))
                          acc))))))
      ;; The body.
      `(progn
        (defclass ,name ,superclasses () (:metaclass vector-class))
        ,@(rec shader*
               `(loop :for c :in (class-list (find-class type))
                      :for slots = (c2mop:class-direct-slots c)
                      :when slots
                        :collect (format nil "vec~D" (length slots))
                        :and :collect (change-case:camel-case
                                        (symbol-name (class-name c))))
               nil)))))

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

(defun make-gl-vector (initial-contents)
  (let* ((length (length initial-contents))
         (a
          (gl:alloc-gl-array
            (foreign-type (array-element-type initial-contents) :cffi t)
            length)))
    (dotimes (i length a) (setf (gl:glaref a i) (aref initial-contents i)))))

(defmacro with-gl-vector ((&rest bind*) &body body)
  `(let ,(mapcar
           (lambda (bind)
             (check-type bind (cons symbol (cons t null)))
             (destructuring-bind
                 (var vector)
                 bind
               `(,var (make-gl-vector ,vector))))
           bind*)
     (unwind-protect (progn ,@body)
       ,@(mapcar (lambda (bind) `(gl:free-gl-array ,(car bind))) bind*))))

;;; WITH-BUFFER

(deftype buffer-usage () '(member :static-draw :stream-draw :dynamic-draw))

(deftype buffer-target ()
  '(member :array-buffer :element-array-buffer
           :copy-read-buffer :copy-write-buffer
           :pixel-unpack-buffer :pixel-pack-buffer
           :query-buffer :texture-buffer
           :transform-feedback-buffer :uniform-buffer
           :draw-indirect-buffer :atomic-counter-buffer
           :dispatch-indirect-buffer :shader-storage-buffer))

(defmacro with-buffer ((&rest var*) &body body)
  (assert (every #'symbolp var*))
  `(destructuring-bind
       ,var*
       (gl:gen-buffers ,(length var*))
     (unwind-protect (progn ,@body) (gl:delete-buffers (list ,@var*)))))

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

;;;; WITH-VAO

(defmacro with-vao ((&rest bind*) &body body)
  (let ((table (gensym "TABLE")))
    (flet ((<init-buffer> (clause buf vec)
             (destructuring-bind
                 (&key (target :array-buffer) (usage :static-draw))
                 (cddr clause)
               `((gl:bind-buffer (the buffer-target ,target) ,buf)
                 (gl:buffer-data (the buffer-target ,target)
                                 (the buffer-usage ,usage) ,vec)))))
      `(let ((,table (make-hash-table)))
         (flet ((indices-of (id)
                  (gethash id ,table)))
           ,@(labels ((rec (bind*)
                        (if (endp bind*)
                            body
                            (let ((prog (gensym "PROG"))
                                  (vector (gensym "VECTOR"))
                                  (vertices (gensym "VERTICES"))
                                  (indices (gensym "INDICES"))
                                  (bufs (alexandria:make-gensym-list 2)))
                              (check-type (car bind*) (cons symbol (cons *)))
                              (assert (every
                                        (lambda (x) (assoc x (cdar bind*)))
                                        '(:vertices :indices :attributes
                                          :shader)))
                              `((with-prog (,prog
                                            ,@(cdr
                                                (assoc :shader (cdar bind*))))
                                  (let ((,vector
                                         ,(second
                                            (assoc :indices (cdar bind*)))))
                                    (with-gl-vector ((,vertices
                                                      ,(second
                                                         (assoc :vertices (cdar
                                                                            bind*))))
                                                     (,indices ,vector))
                                      (with-buffer ,bufs
                                        (with-vertex-array ((,(caar bind*)
                                                             ,@(<init-buffer>
                                                                 (assoc
                                                                   :vertices (cdar
                                                                               bind*))
                                                                 (car bufs)
                                                                 vertices)
                                                             (link-attributes
                                                               ,(second
                                                                  (assoc
                                                                    :attributes (cdar
                                                                                  bind*)))
                                                               ,prog)
                                                             ,@(<init-buffer>
                                                                 (assoc
                                                                   :indices (cdar
                                                                              bind*))
                                                                 (cadr bufs)
                                                                 indices)))
                                          (setf (gethash ,(caar bind*) ,table)
                                                  ,vector)
                                          ,@(rec (cdr bind*))))))))))))
               (rec bind*)))))))

;;; WITH-SHADER

(defmacro with-shader ((&rest binds) &body body)
  (let* ((length (length binds))
         (array-vars (alexandria:make-gensym-list length))
         (buf-vars (alexandria:make-gensym-list length)))
    `(with-gl-vector ,(mapcar (lambda (b g) `(,g ,(cadr b))) binds array-vars)
       (with-buffer ,(mapcar #'list buf-vars)
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
                                ,@(let ((uniforms (getf (car binds) :uniform)))
                                    (if uniforms
                                        `((let ,(mapcar
                                                  (lambda (uniform)
                                                    (destructuring-bind
                                                        (var . original)
                                                        (uiop:ensure-list
                                                          uniform)
                                                      `(,var
                                                        (gl:get-uniform-location
                                                          ,(caar binds)
                                                          ,(if original
                                                               (change-case:camel-case
                                                                 (symbol-name
                                                                   (car
                                                                     original)))
                                                               (change-case:camel-case
                                                                 (symbol-name
                                                                   var)))))))
                                                  uniforms)
                                            ,@(rec (cdr binds))))
                                        (rec (cdr binds))))))))))
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

;;;; WITH-CLEAR

(deftype buffer-bit ()
  '(member :color-buffer-bit :depth-buffer-bit :stencil-buffer-bit))

(defmacro with-clear
          ((var-win (&rest bufs) &key (color ''(1.0 1.0 1.0 1.0))) &body body)
  `(progn
    (apply #'gl:clear-color ,color)
    (gl:clear ,@(mapcar (lambda (buf) `(the buffer-bit ,buf)) bufs))
    ,@body
    (sdl2:gl-swap-window ,var-win)))

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
                            "~:>~^ ~:_")
                      "~@{~W~^ ~_~}" ; body
                      "~:>"))))
    stream exp))

(set-pprint-dispatch '(cons (member with-clear)) 'pprint-with-clear)