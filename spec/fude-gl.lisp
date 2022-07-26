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
