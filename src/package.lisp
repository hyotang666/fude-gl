(in-package :cl-user)

(defpackage :fude-gl
  (:use :cl)
  (:export ;;;; VERTEX-ATTRIBUTES
           #:define-vertex-attribute ; dsl macro.
           #:list-all-attributes ; dev helper.
           ;; Attributes classes.
           #:xy
           #:xyz
           #:st
           #:rgb
           #:offset
           #:a
           ;;;; SHADERS
           #:defshader ; dsl macro.
           ;; Dev helper generic functions.
           #:vertex-shader
           #:fragment-shader
           #:uniforms
           ;; UNIFORM
           #:uniform
           #:with-uniforms
           #:send
           ;;;; BUFFER
           #:buffer ; object
           #:in-buffer
           ;; readers.
           #:buffer-target
           #:buffer-source
           ;;;; VERTEX-ARRAY
           #:vertex-array
           #:in-vertex-array
           ;;;; VERTICES
           #:defvertices ; dsl macro.
           #:with-shader ; cleanup.
           #:in-vertices
           #:find-vertices
           #:shader ; reader
           #:instances-buffer ; reader
           #:draw
           #:list-all-vertices ; dev helpers.
           ;;;; TEXTURE
           #:deftexture ; dsl macro.
           #:in-texture
           #:list-all-textures
           #:find-texture
           ;;;; FRAMEBUFFER
           #:deframebuf ; dsl macro.
           #:with-framebuffer ; cleanup.
           #:framebuffer-texture
           #:find-framebuffer
           ;;;; TEXT-RENDERING
           #:with-text
           #:render-text
           #:glyph ; shader-class
           #:with-glyph
           #:*font-size*
           #:font-loader
           ;;;; CAMERA
           ;; constructor
           #:make-camera
           ;; readers
           #:camera-position
           #:camera-front
           #:camera-up
           ;; helpers
           #:view
           #:move
           ;;;; UTILITIES
           #:with-clear
           #:tex-image-2d
           ;;;; DELTA-TIME
           #:with-delta-time
           #:make-delta-time
           #:*delta*
           ;;;; MATRIX
           #:radians
           #:ortho
           #:model-matrix
           #:reload
           #:+meye4+
           ;;;; IMAGE-LOADER
           #:defimage ; dsl macro.
           #:list-all-images ; dev helpers.
           #:image ; loader.
           ))