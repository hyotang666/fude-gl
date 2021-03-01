; vim: ft=lisp et
(in-package :asdf)
(defsystem "fude-gl-examples"
  :version
  "0.5.1"
  :depends-on
  (
   "fude-gl"
   "sdl2" ; window manager.
   "opticl" ; image loader.
   )
  :pathname
  "examples/"
  :components
  ((:file "examples")))
