(defpackage :sdl2.kit
  (:use #:cl #:alexandria)
  (:nicknames #:kit.sdl2)
  (:export

   ;; general
   #:*event*

   ;; main-loop
   #:start #:quit

   ;; window
   #:window #:gl-window

   #:sdl-window #:sdl-window-id #:gl-context #:window-from-id

   #:all-windows

   #:window-size

   #:window-event
   #:keyboard-event
   #:mousemotion-event
   #:mousebutton-event
   #:mousewheel-event
   #:textinput-event

   #:render #:idle-render #:close-window
   #:other-event))
