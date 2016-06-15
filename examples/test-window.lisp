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
    (let* ((interval 5)
           (current-time (get-internal-real-time))
           (seconds (/ (- current-time start-time) internal-time-units-per-second))
           (fps (float (/ frames interval))))
      (incf frames)
      (when (and (> seconds interval)
                 (plusp fps))
        (format t "Framerate: ~,3f fps, ~,3f ms/frame~%" fps (/ 1000 fps))
        (setf frames 0
              start-time current-time)))))

(defmethod textinput-event :after ((window test-window) ts text)
  (when (string= "Q" (string-upcase text))
    (close-window window)))

(defmethod keyboard-event :after ((window test-window) state ts repeat-p keysym)
  (let ((scancode (sdl2:scancode keysym)))
    (when (eq :scancode-escape scancode)
      (close-window window))))
