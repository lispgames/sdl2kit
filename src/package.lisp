(in-package :defpackage+-user-1)

(defpackage+ :kit.sdl2
  (:use #:cl #:alexandria)
  (:nicknames #:sdl2.kit) ; deprecated
  (:export

   ;; general
   #:*event*

   ;; main-loop
   #:start #:quit #:with-start

   ;; window
   #:window #:gl-window

   #:sdl-window #:sdl-window-id #:gl-context #:window-from-id
   #:render-enabled

   #:all-windows

   #:window-size

   #:window-event
   #:keyboard-event
   #:mousemotion-event
   #:mousebutton-event
   #:mousewheel-event
   #:textinput-event
   #:controller-added-event
   #:controller-removed-event
   #:controller-axis-motion-event
   #:controller-button-event

   #:render #:idle-render #:close-window
   #:other-event

   ;; keyboard
   #:keystate-tracker
   #:key-down-p #:keystate #:keystate-update
   ))
