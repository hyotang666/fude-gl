(defpackage :fude-gl.spec
  (:use :cl :jingoh :fude-gl))
(in-package :fude-gl.spec)
(setup :fude-gl)

(requirements-about GLSL-SYMBOL :doc-type function)

;;;; Description:
; Print a symbol EXP to STREAM as GLSL code.
; If EXP is unknown for compiler and ERRORP is true, condition UNKNOWN-VARIABLE is signaled
; otherwise the condition is not signaled. The default is the value of *VAR-CHECK-P*.
; You can specify this by colon in format control.
; If EXP is known for compiler and NOT-REF-P is ture, compiler memos it is refered
; otherwise compiler do nothing. The default it NIL. You can specify this by at-sign in format control.

#+syntax (GLSL-SYMBOL STREAM EXP &OPTIONAL (ERRORP *VAR-CHECK-P*) NOT-REF-P)
; => result

;;;; Arguments and Values:

; stream := 

; exp := 

; errorp := 

; not-ref-p := 

; result := 

;;;; Affected By:
; eprot:*environment*.

;;;; Side-Effects:
; Modify eprot:*environment* for reference checking when NOT-REF-P is NIL.
#?(let ((eprot:*environment*
	  (eprot:augment-environment
	    (eprot:find-environment :fude-gl)
	    :variable '(known-var)
	    :declare '((glsl-env:notation known-var "This is printed.")))))
    (values (assoc 'ignorable (nth-value 2 (fude-gl::variable-information
					     'known-var eprot:*environment*)))
	    (progn
	      (fude-gl::glsl-symbol (make-broadcast-stream) 'known-var)
	      (assoc 'ignorable (nth-value 2 (fude-gl::variable-information
					       'known-var eprot:*environment*))))))
:values (NIL (IGNORABLE . T))

; Does reference memo works with nested environment?
#?(let ((eprot:*environment*
	  (eprot:augment-environment
	    (eprot:find-environment :fude-gl)
	    :name "Outer"
	    :variable '(known-var)
	    :declare '((glsl-env:notation known-var "This is printed.")))))
    (let ((eprot:*environment*
	    (eprot:augment-environment
	      eprot:*environment*
	      :name "Inner"
	      :variable '(dummy))))
      (fude-gl::glsl-symbol (make-broadcast-stream) 'known-var))
    ;; after exit scope of inner env.
    (assoc 'ignorable (nth-value 2 (fude-gl::variable-information
				     'known-var eprot:*environment*))))
=> (IGNORABLE . T)
,:test equal

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Tests.
; Case EXP is unknown var.
#?(fude-gl::glsl-symbol nil 'unknown-var)
:outputs "unknownVar"

; Case EXP is unknown var with ERRORP is true.
#?(fude-gl::glsl-symbol nil 'unknown-var t) :signals fude-gl::unknown-variable

; Case EXP is known var.
#?(let ((eprot:*environment*
	  (eprot:augment-environment
	    (eprot:find-environment :fude-gl)
	    :variable '(known-var)
	    :declare '((glsl-env:notation known-var "This is printed.")))))
    (fude-gl::glsl-symbol nil 'known-var))
:outputs "This is printed."

(requirements-about GLSL-DECLAIM :doc-type function)

;;;; Description:

#+syntax (GLSL-DECLAIM STREAM EXP) ; => result

#?(let ((eprot:*environment*
	  (eprot:augment-environment
	    (eprot:find-environment :fude-gl))))
    (fude-gl::glsl-declaim *standard-output*
			   '(declaim (ftype (function () (values)) new-fn))))
:outputs ":VOID NEW-FN();"

;;;; Arguments and Values:

; stream := 

; exp := 

; result := 

;;;; Affected By:

;;;; Side-Effects:
; eprot:*environment*.
#?(let ((eprot:*environment*
	  (eprot:augment-environment
	    (eprot:find-environment :fude-gl))))
    (values (fude-gl::function-information 'no-such eprot:*environment*)
	    (progn (fude-gl::glsl-declaim
		     (make-broadcast-stream)
		     '(declaim (ftype (function () (values)) no-such)))
		   (fude-gl::function-information 'no-such eprot:*environment*))))
:values (NIL :FUNCTION)

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about GLSL-SETF :doc-type function)

;;;; Description:

#+syntax (GLSL-SETF STREAM EXP) ; => result

;;;; Arguments and Values:

; stream := 

; exp := 

; result := 

;;;; Affected By:
; *print-pprint-dispatch*
; eprot:*environment*

;;;; Side-Effects:
; Modify eprot:*environment*

;;;; Notes:

;;;; Exceptional-Situations:
; Case unknown var as place.
#?(let ((eprot:*environment*
	  (eprot:augment-environment
	    (eprot:find-environment :fude-gl))))
    (fude-gl::print-glsl '(setf unknown-place 0)
			 (make-broadcast-stream)))
:signals fude-gl::unknown-variable

; Case known var as place.
#?(let ((eprot:*environment*
	  (eprot:augment-environment
	    (eprot:find-environment :fude-gl)
	    :variable '(known)
	    :declare '((glsl-env:notation known "Known")))))
    (fude-gl::print-glsl '(setf known 0)))
:outputs "
Known = 0"

; Side effect: Setfable place is refered.
#?(let ((eprot:*environment*
	  (eprot:augment-environment
	    (eprot:find-environment :fude-gl)
	    :variable '(known)
	    :declare '((glsl-env:notation known "Known")))))
    (fude-gl::print-glsl '(setf known 0) (make-broadcast-stream))
    (fude-gl::variable-information 'known eprot:*environment*))
:multiple-value-satisfies
(lambda (type lexicalp info)
  (& (eq :lexical type)
     (eq t lexicalp)
     (null (set-difference '((glsl-env:notation . "Known") (ignorable . t))
			   info
			   :test #'equal))))

; Case unknown var in the right side.
#?(let ((eprot:*environment*
	  (eprot:augment-environment
	    (eprot:find-environment :fude-gl)
	    :variable '(known)
	    :declare '((glsl-env:notation known "Known")))))
    (fude-gl::print-glsl '(setf known unknown)))
:signals fude-gl::unknown-variable

; Case known var in the right side.
#?(let ((eprot:*environment*
	  (eprot:augment-environment
	    (eprot:find-environment :fude-gl)
	    :variable '(known)
	    :declare '((glsl-env:notation known "Known")))))
    (fude-gl::print-glsl '(setf gl-position known)))
:outputs "
gl_Position = Known"

; Side-effect: Right side var is refered.
#?(let ((eprot:*environment*
	  (eprot:augment-environment
	    (eprot:find-environment :fude-gl)
	    :variable '(known)
	    :declare '((glsl-env:notation known "Known")))))
    (fude-gl::print-glsl '(setf gl-position known) (make-broadcast-stream))
    (fude-gl::variable-information 'known eprot:*environment*))
:multiple-value-satisfies
(lambda (type lexicalp info)
  (& (eq :lexical type)
     (eq t lexicalp)
     (null (set-difference info
			   '((glsl-env:notation . "Known")
			     (ignorable . t))
			   :test #'equal))))

