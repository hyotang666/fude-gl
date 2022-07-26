; vim: ft=lisp et
(in-package :asdf)
(defsystem "fude-gl.test"
  :version
  "0.1.0"
  :depends-on
  (:jingoh "fude-gl")
  :components
  ((:file "fude-gl"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :fude-gl args)))
