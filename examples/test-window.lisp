;;;
;;; This is just a basic window which does some convenient things for
;;; tests, such as FPS and quitting on ESC.
;;;

(in-package :kit.sdl2.test)

(defclass test-window (gl-window)
  ((start-time :initform (get-internal-real-time))
   (frames :initform 0)))

(defmethod render :after ((window test-window))
  (with-slots (start-time frames) window
    (incf frames)
    (let* ((current-time (get-internal-real-time))
           (seconds (/ (- current-time start-time) internal-time-units-per-second)))
      (when (> seconds 5)
        (format t "FPS: ~A~%" (float (/ frames seconds)))
        (setf frames 0)
        (setf start-time (get-internal-real-time))))))

(defmethod textinput-event :after ((window test-window) ts text)
  (when (string= "Q" (string-upcase text))
    (close-window window)))

(defmethod keyboard-event :after ((window test-window) state ts repeat-p keysym)
  (let ((scancode (sdl2:scancode keysym)))
    (when (eq :scancode-escape scancode)
      (close-window window))))
