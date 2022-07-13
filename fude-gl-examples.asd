; vim: ft=lisp et
(in-package :asdf)
(defsystem "fude-gl-examples"
  :version
  "0.36.6"
  :depends-on
  (
   "fude-gl"
   "sdl2" ; window manager.
   "opticl" ; image loader.
   "3d-matrices" ; Matrix operations.
   "dexador" ; http client.
   "classimp"   ; ASSet-IMPorter.
   )
  :pathname
  "examples/"
  :components
  ((:file "examples")))
