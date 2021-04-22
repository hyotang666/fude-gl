; vim: ft=lisp et
(in-package :asdf)
(defsystem "fude-gl"
  :version
  "2.22.1"
  :description
  "Fundamental Utility Definitions Especially for openGL."
  :license "MIT"
  :author "SATO Shinichi"
  :depends-on
  (
   "cl-opengl" ; FFI for opengl.
   "closer-mop" ; Wrapper for metaobject protocols.
   "uiop" ; Utilities.
   "alexandria" ; Public domain utilities.
   "cl-change-case" ; Convert lisp case to camel case.
   "sdl2" ; Windowing support.
   "millet" ; Wrapper for tiny utilities.
   "check-bnf" ; BNF like macro syntax checker.
   "vecto" ; TTF rasterization.
   "zpb-ttf" ; TTF font loader.
   "3d-matrices" ; Matrix operations.
   "opticl" ; Image file loader.
   )
  :pathname
  "src/"
  :components
  ((:file "package")
   (:file "glsl" :depends-on ("package"))
   (:file "fude-gl" :depends-on ("glsl"))
   (:file "text" :depends-on ("fude-gl"))
   ))

;;; These forms below are added by JINGOH.GENERATOR.
;; Ensure in ASDF for pretty printings.
(in-package :asdf)
;; Enable testing via (asdf:test-system "fude-gl").
(defmethod component-depends-on ((o test-op) (c (eql (find-system "fude-gl"))))
  (append (call-next-method) '((test-op "fude-gl.test"))))
;; Enable passing parameter for JINGOH:EXAMINER via ASDF:TEST-SYSTEM.
(defmethod operate :around
           ((o test-op) (c (eql (find-system "fude-gl")))
            &rest keys
            &key ((:compile-print *compile-print*))
            ((:compile-verbose *compile-verbose*)) &allow-other-keys)
  (flet ((jingoh.args (keys)
           (loop :for (key value) :on keys :by #'cddr
                 :when (find key '(:on-fails :subject :vivid) :test #'eq)
                 :collect key
                 :and
                 :collect value :else
                 :when (eq :jingoh.verbose key)
                 :collect :verbose
                 :and
                 :collect value)))
    (let ((args (jingoh.args keys)))
      (declare (special args))
      (call-next-method))))
;; Enable importing spec documentations.
(let ((system (find-system "jingoh.documentizer" nil)))
  (when system
    (load-system system)
    (defmethod perform :after ((o load-op) (c (eql (find-system "fude-gl"))))
      (with-muffled-conditions (*uninteresting-conditions*)
        (handler-case (symbol-call :jingoh.documentizer :import c)
                      (error (condition)
                             (warn "Fails to import documentation of ~S.~%~A"
                                   (coerce-name c)
                                   (princ-to-string condition))))))))
