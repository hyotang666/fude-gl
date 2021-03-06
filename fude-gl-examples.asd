; vim: ft=lisp et
(in-package :asdf)
(defsystem "fude-gl-examples"
  :version
  "0.7.2"
  :depends-on
  (
   "fude-gl"
   "sdl2" ; window manager.
   "opticl" ; image loader.
   "sb-cga" ; Matrix operations.
   )
  :pathname
  "examples/"
  :components
  ((:file "examples")))
