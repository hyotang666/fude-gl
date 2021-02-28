; vim: ft=lisp et
(in-package :asdf)
(defsystem "fude-gl-examples"
  :version
  "0.2.0"
  :depends-on
  (
   "fude-gl"
   "sdl2"
   )
  :pathname
  "examples/"
  :components
  ((:file "examples")))
