(in-package :kit.sdl2)

(defclass keystate-tracker ()
  ((keystate :initform (make-array 300 :element-type 'bit))))

(defun keystate-update (tracker state repeat-p keysym)
  (with-slots (keystate) tracker
    (when (and state (not repeat-p))
      (case state
        (:keydown (setf (aref keystate (sdl2:scancode-value keysym)) 1))
        (:keyup   (setf (aref keystate (sdl2:scancode-value keysym)) 0))))))

(defun keystate (tracker scancode)
  (let ((num (autowrap:enum-value 'sdl2-ffi:sdl-scancode scancode)))
    (with-slots (keystate) tracker
      (aref keystate num))))

(defun key-down-p (tracker scancode)
  (= 1 (keystate tracker scancode)))




