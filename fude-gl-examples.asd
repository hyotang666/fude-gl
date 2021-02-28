; vim: ft=lisp et
(in-package :asdf)
(defsystem "fude-gl-examples"
  :version
  "0.1.0"
  :depends-on
  (
   "fude-gl"
   "sdl2"
   )
  :pathname
  "examples/"
  :components
  ((:file "examples")))
