(asdf:defsystem #:sdl2kit
  :serial t
  :description "A utility kit for SDL2"
  :author "Chip Collier <photex@lofidelitygames.com>, Ryan Pavlik <rpavlik@gmail.com>, Peter Keller <psilord@cs.wisc.edu>"
  :license "MIT"

  :depends-on (:alexandria :sdl2 :defpackage-plus)
  :pathname "src"
  :serial t

  :components
  ((:file "package")
   (:file "window")
   (:file "keyboard")
   (:file "main")))

