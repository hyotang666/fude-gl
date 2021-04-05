; vim: ft=lisp et
(in-package :asdf)
(defsystem "fude-gl-examples"
  :version
  "0.20.0"
  :depends-on
  (
   "fude-gl"
   "sdl2" ; window manager.
   "opticl" ; image loader.
   "3d-matrices" ; Matrix operations.
   "dexador" ; http client.
   )
  :pathname
  "examples/"
  :components
  ((:file "examples")))
