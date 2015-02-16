(asdf:defsystem #:sdl2kit-examples
  :serial t
  :description "sdl2kit examples"
  :author "Chip Collier <photex@lofidelitygames.com>, Ryan Pavlik <rpavlik@gmail.com>, Peter Keller <psilord@cs.wisc.edu>"
  :license "MIT"

  :depends-on (:alexandria :sdl2kit)
  :pathname "examples"
  :serial t

  :components
  ((:file "test-window")
   (:file "sdl2kit")))

