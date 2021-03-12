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
           #:foreign-type
           #:indices-of
           #:draw-elements
           #:tex-image-2d))

(in-package :fude-gl)

;;;; MATRIX

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
  (:documentation "Return vertex shader code string."))

(defgeneric fragment-shader (name)
  (:documentation "Return fragment shader code string."))

(defgeneric uniforms (name)
  (:documentation "Return associated uniform symbols."))

;;;; DSL
;;; DEFSHADER

(defun <uniforms> (name shader*)
  `(defmethod uniforms ((type (eql ',name)))
     (list
       ,@(loop :for (nil lambda-list) :in shader*
               :for position
                    = (position-if
                        (lambda (x) (and (symbolp x) (string= '&uniform x)))
                        lambda-list)
               :when position
                 :nconc (let ((acc))
                          (dolist (x (subseq lambda-list (1+ position)) acc)
                            (pushnew `',(car x) acc :test #'equal)))))))

(defun <shader-method> (method name main string)
  `(defmethod ,method ((type (eql ',name)))
     ,(if (typep main '(cons (cons (eql quote) (cons symbol null)) null))
          `(,method ',(cadar main))
          string)))

(defmacro defshader (name version superclasses &body shader*)
  ;; Trivial syntax check.
  (check-type name symbol)
  (check-type version (or symbol integer))
  (assert (and (listp superclasses) (every #'find-class superclasses)))
  (assert (every (lambda (s) (find (car s) '(:vertex :fragment))) shader*))
  ;; binds
  (let ((format
         (formatter
          #.(concatenate 'string "#version ~A core~%" ; version
                         "~{in ~A ~A;~%~}~&" ; in
                         "~{out ~A ~A;~%~}~&" ; out
                         "~@[~{uniform ~A ~A;~%~}~]~&" ; uniforms
                         "void main () {~%~{~A~^~%~}~%}" ; the body.
                         ))))
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
                   (rec rest vars
                        (cons
                          (<shader-method>
                            (intern (format nil "~A-SHADER" type) :fude-gl)
                            name main
                            (format nil format version in vars
                                    (and &uniform
                                         (defs (subseq out (1+ &uniform))))
                                    main))
                          acc))))))
      ;; The body.
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (defclass ,name ,superclasses () (:metaclass vector-class))
         ,@(rec shader*
                (loop :for c
                           :in (mapcan (lambda (c) (class-list (find-class c)))
                                       superclasses)
                      :for slots = (c2mop:class-direct-slots c)
                      :when slots
                        :collect (format nil "vec~D" (length slots))
                        :and :collect (change-case:camel-case
                                        (symbol-name (class-name c))))
                nil)
         ,(<uniforms> name shader*)
         ',name))))

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
  (alexandria:with-unique-names (vs fs compile warn)
    `(let ((,var (gl:create-program)))
       (labels ((,compile (id source)
                  (gl:shader-source id source)
                  (gl:compile-shader id)
                  (,warn (gl:get-shader-info-log id))
                  (gl:attach-shader ,var id))
                (,warn (log)
                  (unless (equal "" log)
                    (warn log))))
         (unwind-protect
             (let ((,vs (gl:create-shader :vertex-shader))
                   (,fs (gl:create-shader :fragment-shader)))
               (unwind-protect
                   (progn
                    (,compile ,vs ,vertex-shader)
                    (,compile ,fs ,fragment-shader)
                    (gl:link-program ,var)
                    (,warn (gl:get-program-info-log ,var))
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
                 #++
                 (uiop:format! *trace-output* "~%Length ~S. Offset ~S."
                               (* total-length size) (* offset size))
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

(deftype texture-wrapping ()
  '(member :repeat :mirrored-repeat :clamp-to-edge :clamp-to-border))

(deftype texture-target ()
  '(member :texture-1d :texture-1d-array
           :texture-2d :texture-2d-array
           :texture-2d-multisample :texture-2d-multisample-array
           :texture-3d :texture-cube-map
           :texture-cube-map-array :texture-rectangle))

(deftype texture-pname ()
  '(member :depth-stencil-texture-mode
           :texture-base-level :texture-compare-func
           :texture-compare-mode :texture-lod-bias
           :texture-min-filter :texture-mag-filter
           :texture-min-lod :texture-max-lod
           :texture-max-level :texture-swizzle-r
           :texture-swizzle-g :texture-swizzle-b
           :texture-swizzle-a :texture-wrap-s
           :texture-wrap-t :texture-wrap-r))

(deftype texture-mag-filter () '(member :linear :nearest))

(deftype texture-min-filter ()
  '(or textute-mag-filter
       (member :nearest-mipmap-nearest :lenear-mipmap-nearest
               :nearest-mipmap-linear :linear-mipmap-linear)))

(deftype base-internal-format ()
  '(member :depth-component :depth-stencil :red :rg :rgb :rgba))

(deftype pixel-format ()
  '(or base-internal-format
       (member :bgr
               :bgra :red-integer
               :rg-integer :rgb-integer
               :bgr-integer :rgba-integer
               :bgra-integer :stencil-index)))

(defvar *active-counter* -1)

(defmacro with-textures ((&rest bind*) &body body)
  ;; Trivial syntax check.
  (dolist (b bind*) (the (cons symbol (cons texture-target *)) b))
  (labels ((vname (k v)
             (case k
               ((:texture-wrap-s :texture-wrap-t :texture-wrap-r)
                (ensure-check v 'texture-wrapping))
               ((:texture-mag-filter) (ensure-check v 'texture-mag-filter))
               ((:texture-min-fileter) (ensure-check v 'texture-min-filter))
               (otherwise v)))
           (<option-setters> (params target)
             (destructuring-bind
                 (&key (texture-wrap-s :repeat) (texture-wrap-t :repeat)
                  (texture-min-filter :linear) (texture-mag-filter :linear)
                  &allow-other-keys)
                 params
               (let ((params
                      (list* :texture-wrap-s texture-wrap-s :texture-wrap-t
                             texture-wrap-t :texture-mag-filter
                             texture-mag-filter :texture-min-filter
                             texture-min-filter
                             (uiop:remove-plist-keys
                               '(:texture-wrap-s :texture-wrap-t
                                 :texture-min-filter :texture-mag-filter)
                               params))))
                 (loop :for (k v) :on params :by #'cddr
                       :collect `(gl:tex-parameter ,target
                                                   ,(ensure-check k
                                                                  'texture-pname)
                                                   ,(vname k v))))))
           (ensure-check (v type)
             (if (constantp v)
                 (progn (assert (typep v type)) v)
                 `(the ,type ,v))))
    ;; The body.
    `(destructuring-bind
         ,(mapcar #'car bind*)
         (gl:gen-textures ,(length bind*))
       (unwind-protect
           (progn
            ,@(mapcan
                (lambda (b)
                  (destructuring-bind
                      (var target &key params init (uniform 0))
                      b
                    `((gl:active-texture ,var)
                      (gl:bind-texture ,(ensure-check target 'texture-target)
                                       ,var)
                      ,@(<option-setters> params target) ,init
                      (gl:uniformi ,uniform ,var))))
                bind*)
            ,@body)
         (gl:delete-textures (list ,@(mapcar #'car bind*)))))))

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
                                  "~W~^ ~:I~@_" ; var
                                  "~W~^ ~_" ; target.
                                  "~@{~W~^ ~@_~W~^ ~_~}" ; k-v options.
                                  "~:>~^ ~_")
                            "~}" ; end of iterate.
                            "~:>~^ ~:@_")
                      "~@{~W~^ ~_~}" ; the body.
                      "~:>"))))
    stream exp))

(set-pprint-dispatch '(cons (member with-textures)) 'pprint-with-textures)

(defun tex-image-2d (array)
  (let ((format (ecase (array-dimension array 2) (3 :rgb) (4 :rgba))))
    (gl:tex-image-2d (the texture-target :texture-2d) 0 ; mipmap level.
                     (the base-internal-format format)
                     (array-dimension array 0) ; width
                     (array-dimension array 1) ; height
                     0 ; legacy stuff.
                     (the pixel-format format)
                     (foreign-type (array-element-type array))
                     (make-array (array-total-size array)
                                 :element-type (array-element-type array)
                                 :displaced-to array))))

;;;; WITH-VAO

(defmacro indices-of (id)
  (declare (ignore id))
  (error "INDICE-OF is must be inside of WITH-VAO."))

(defun <uniform-binder> (prog)
  (lambda (uniform)
    (etypecase uniform
      (symbol
       `(,uniform
         (gl:get-uniform-location ,prog
                                  ,(change-case:camel-case
                                     (symbol-name uniform)))))
      ((cons symbol (cons symbol null))
       `(,(first uniform)
         (gl:get-uniform-location ,prog
                                  ,(change-case:camel-case
                                     (symbol-name (second uniform)))))))))

(defun ensure-second (thing)
  (if (listp thing)
      (second thing)
      thing))

(defun type-assert (form type)
  (if (constantp form)
      (progn
       (assert (typep form type) ()
         "~S is not type of ~S" form (millet:type-expand type))
       form)
      `(the ,type ,form)))

(defmacro with-vao ((&rest bind*) &body body)
  (let ((table (gensym "TABLE")))
    (labels ((<init-buffer> (clause buf vec)
               (destructuring-bind
                   (&key (target :array-buffer) (usage :static-draw))
                   (cddr clause)
                 `((gl:bind-buffer ,(type-assert target 'buffer-target) ,buf)
                   (gl:buffer-data ,(type-assert target 'buffer-target)
                                   ,(type-assert usage 'buffer-usage) ,vec))))
             (clause (clause bind)
               (or (assoc clause (cdr bind))
                   (error "Missing required cluase ~S in ~S" clause bind)))
             (rec (bind*)
               (if (endp bind*)
                   body
                   (let ((prog (gensym "PROG")))
                     `((with-prog (,prog ,@(cdr (clause :shader (car bind*))))
                         ,(body (assoc :indices (cdar bind*)) prog bind*))))))
             (<may-uniform-bind> (uniforms bind*)
               (if uniforms
                   `((let ,uniforms
                       ,@(rec (cdr bind*))))
                   (rec (cdr bind*))))
             (<body-form>
                 (bind* prog
                  &optional indices-bind ebo-bind ebo-inits table-inits)
               (let ((vertices (gensym "VERTICES"))
                     (vbo (gensym "VBO"))
                     (uniforms
                      (let* ((uniforms (cdr (assoc :uniform (cdar bind*))))
                             (required (uniforms (caar bind*)))
                             (actual (mapcar #'ensure-second uniforms)))
                        (assert (null (set-exclusive-or required actual)) ()
                          "Mismatch uniforms. ~S but ~S" required actual)
                        (mapcar (<uniform-binder> prog) uniforms)))
                     (verts (clause :vertices (car bind*)))
                     (attr (second (clause :attributes (car bind*)))))
                 (check-type (car bind*) (cons symbol (cons *)))
                 `(with-gl-vector ((,vertices ,(second verts)) ,@indices-bind)
                    (with-buffer ,(list* vbo ebo-bind)
                      (with-vertex-array ((,(caar bind*)
                                           ,@(<init-buffer> verts vbo vertices)
                                           (link-attributes ,attr ,prog)
                                           ,@ebo-inits))
                        ,@table-inits
                        ,@(<may-uniform-bind> uniforms bind*))))))
             (body (vec prog bind*)
               (if vec
                   (alexandria:with-unique-names (vector indices ebo)
                     `(let ((,vector ,(second vec)))
                        ,(<body-form> bind* prog `((,indices ,vector))
                                      (list ebo)
                                      (<init-buffer> vec ebo indices)
                                      `((setf (gethash ,(caar bind*) ,table)
                                                ,vector)))))
                   (<body-form> bind* prog))))
      `(let ((,table (make-hash-table)))
         (macrolet ((indices-of (id)
                      `(or (gethash ,id ,',table)
                           (error "No indices for ~S." ,id))))
           ,@(rec bind*))))))

;;; WITH-SHADER

(defmacro with-shader ((&rest bind*) &body body)
  (let ((uniform-vars
         (alexandria:make-gensym-list
           (loop :for (nil . clause*) :in bind*
                 :sum (loop :for c :in clause*
                            :when (eq :uniform (car c))
                              :sum (count-if #'listp (cdr c)))))))
    `(with-vao ,(mapcar
                  (lambda (bind)
                    (destructuring-bind
                        (class &rest clause*)
                        bind
                      `(,class
                        ,@(loop :for clause :in clause*
                                :when (eq :indices (car clause))
                                  :collect `(:indices
                                             (coerce ,(second clause)
                                                     '(array (unsigned-byte 8)
                                                       (*)))
                                             :target :element-array-buffer)
                                :when (eq :uniform (car clause))
                                  :collect `(:uniform
                                             ,@(mapcar #'alexandria:ensure-car
                                                       (cdr clause)))
                                :else
                                  :collect clause)
                        (:attributes ',class)
                        (:shader (vertex-shader ',class)
                         (fragment-shader ',class)))))
                  bind*)
       ,@(let ((uniforms
                (mapcan
                  (lambda (bind)
                    (remove-if #'symbolp (cdr (assoc :uniform (cdr bind)))))
                  bind*)))
           (if (null uniforms)
               body
               `((with-textures ,(mapcar
                                   (lambda (uniform gvar)
                                     (destructuring-bind
                                         (var target init)
                                         uniform
                                       `(,gvar ,target :init ,init :uniform
                                         ,var)))
                                   uniforms uniform-vars)
                   ,@body)))))))

(defun pprint-with-shader (stream exp)
  (funcall
    (formatter
     #.(apply #'concatenate 'string
              (alexandria:flatten
                (list "~:<" ; Pprint-logical-block.
                      "~W~^ ~1I" ; Operator.
                      (list "~:<" ; Binds
                            (list "~:<" ; Each bind clause.
                                  "~@{" ; Clause.
                                  "~W~^ ~1I~_" ; Var
                                  "~@{" ; Each options
                                  (list "~:<" ; option.
                                        "~W~^ ~:I~@_" ; option key.
                                        "~@{~W~^ ~:_~}" ; option body.
                                        "~:>~^ ~_")
                                  "~}" ; Options.
                                  "~}" ; End clause.
                                  "~:>")
                            "~:>~^ ~_")
                      "~@{~W~^ ~_~}" ; Body.
                      "~:>"))))
    stream exp))

(set-pprint-dispatch '(cons (member with-shader)) 'pprint-with-shader)

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
          ((var-win (&rest bufs) &key (color ''(0.0 0.0 0.0 1.0))) &body body)
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

;;;; DRAW-ELEMENTS

(deftype draw-mode ()
  '(member :points :line-strip
           :line-loop :lines
           :line-strip-adjacency :lines-adjacency
           :triangle-strip :triangle-fan
           :triangles :tiangle-strip-adjacency
           :triangles-adjacency :patches))

(defun draw-elements (mode cl-vector &key (offset 0))
  (%gl:draw-elements mode (length cl-vector)
                     (foreign-type (array-element-type cl-vector)) offset))
