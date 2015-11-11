;;; HOW TO USE:
;;;
;;; First, run this.  It is SAFE to run repeatedly:
;;;
;;;   (kit.sdl2:start)
;;;
;;; Then, make a window.
;;;
;;;   (make-instance 'kit.sdl2.test:simple-window)
;;;
;;; You can make multiple windows if you want.  Note that, despite not
;;; assigning the value, THIS IS NOT COLLECTED.  A reference is kept
;;; to all windows:
;;;
;;;   (all-windows)
;;;
;;; After you close a window, it will be collected at some point.
;;;
;;; You should NOT call any protocol functions on a window, except the
;;; following:
;;;
;;;   (render WINDOW)
;;;   (close-window WINDOW)
;;;
;;; These are the only functions guaranteed to be "safe" (including
;;; threadsafety and other expectations).

(in-package :kit.sdl2.test)

(defclass simple-window (test-window)
  ((rotation :initform 0.0)))

;;; All of these methods are OPTIONAL.  However, without a render
;;; method, your window will not look like much!


;;; Note this is an :AFTER method.  You should either use :AFTER, or
;;; you must (CALL-NEXT-METHOD).

(defmethod initialize-instance :after ((w simple-window) &key &allow-other-keys)
  ;; GL setup can go here; your GL context is automatically active,
  ;; and this is done in the main thread.
  (setf (idle-render w) t)
  (gl:viewport 0 0 800 600)
  (gl:matrix-mode :projection)
  (gl:ortho -2 2 -2 2 -2 2)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defmethod render ((window simple-window))
  ;; Your GL context is automatically active.  FLUSH and
  ;; SDL2:GL-SWAP-WINDOW are done implicitly by GL-WINDOW
  ;; after RENDER.
  (with-slots (rotation) window
    (gl:load-identity)
    (gl:rotate rotation 0 0 1)
    (gl:clear-color 0.0 0.0 1.0 1.0)
    (gl:clear :color-buffer)
    (gl:begin :triangles)
    (gl:color 1.0 0.0 0.0)
    (gl:vertex 0.0 1.0)
    (gl:vertex -1.0 -1.0)
    (gl:vertex 1.0 -1.0)
    (gl:end)))

(defmethod close-window ((window simple-window))
  (format t "Bye!~%")
  ;; To _actually_ destroy the GL context and close the window,
  ;; CALL-NEXT-METHOD.  You _may_ not want to do this, if you wish to
  ;; prompt the user!
  (call-next-method))

(defmethod mousewheel-event ((window simple-window) ts x y)
  (with-slots (rotation) window
    (incf rotation (* 12 y))
    (render window)))

(defmethod textinput-event ((window simple-window) ts text)
  (format t "You typed: ~S~%" text))

(defmethod keyboard-event ((window simple-window) state ts repeat-p keysym)
  (let ((scancode (sdl2:scancode keysym)))
    (unless repeat-p
      (format t "~A ~S ~S~%" state scancode (sdl2:scancode-name scancode)))))

(defmethod mousebutton-event ((window simple-window) state ts b x y)
  (format t "~A button: ~A at ~A, ~A~%" state b x y))

(defmethod mousemotion-event ((window simple-window) ts mask x y xr yr)
  (when (> mask 0)
    (format t "Mouse motion, button-mask = ~A at ~A, ~A~%" mask x y)))

(defmethod controller-added-event ((window simple-window) c)
  (format t "Added ~A (id=~A)~%" c (sdl2:game-controller-instance-id c)))

(defmethod controller-removed-event ((window simple-window) c)
  (format t "Removed ~A (id=~A)~%" c (sdl2:game-controller-instance-id c)))

(defmethod controller-axis-motion-event ((window simple-window) c ts axis value)
  (format t "ID ~A, Axis ~A, Value ~A~%"
          (sdl2:game-controller-instance-id c) axis value))

(defmethod controller-button-event ((window simple-window) c state ts button)
  (format t "ID ~A, Button ~A, State ~S~%"
          (sdl2:game-controller-instance-id c) button state))

;; (make-instance 'simple-window)
