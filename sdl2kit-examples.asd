(asdf:defsystem #:sdl2kit-examples
  :serial t
  :description "sdl2kit examples"
  :author "Chip Collier <photex@lofidelitygames.com>, Ryan Pavlik <rpavlik@gmail.com>, Peter Keller <psilord@cs.wisc.edu>"
  :license "MIT"

  :depends-on (:alexandria :sdl2kit :defpackage-plus :glkit)
  :pathname "examples"
  :serial t

  :components
  ((:file "package")
   (:file "test-window")
   (:file "sdl2kit")
   (:file "rotating-cube")))

;; TODO: remove
(asdf:load-system "sdl2kit-examples")

(sdl2.kit:start)
(make-instance 'kit.sdl2.test2:test-window)

