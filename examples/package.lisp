(in-package :defpackage+-user-1)

;; KIT.SDL2.TEST

(defpackage+ :kit.sdl2.test
  (:use #:cl #:alexandria #:kit.sdl2)
  (:export #:test-window #:simple-window))

(defpackage+ :kit.sdl2.test2
  (:use #:cl #:alexandria #:kit.sdl2 #:kit.gl.shader #:kit.math)
  (:export #:test-window #:simple-window))
