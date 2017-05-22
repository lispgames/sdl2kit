(in-package :defpackage+-user-1)

(defpackage+ :kit.sdl2
  (:use #:cl #:alexandria)
  (:export-only

   ;; general
   #:*event*

   ;; main-loop
   #:init #:start #:quit #:with-start
   #:define-start-function

   ;; window
   #:window #:gl-window #:initialize-window

   #:sdl-window #:sdl-window-id #:gl-context #:window-from-id
   #:render-enabled

   #:all-windows #:focused-window #:last-window

   #:window-size #:window-position #:window-width #:window-height
   #:window-title

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
