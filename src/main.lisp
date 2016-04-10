(in-package :sdl2.kit)

(defvar *started* nil)
(defvar *main-loop-quit* nil)
(defvar *focused-window-id* nil)
(defvar *last-window-id* nil)

(defvar *event* nil)

(define-condition close-window (condition) ())

(defmacro for-all-windows ((w) &body b)
  (with-gensyms (k)
    `(loop for ,k being each hash-key in *all-windows*
           as ,w = (gethash ,k *all-windows*)
           do (progn ,@b))))

(defmacro when-window-from-id ((var id-form) &body body)
  (with-gensyms (block)
    `(block ,block
       (when-let ((,var (window-from-id ,id-form)))
         (handler-bind ((close-window (lambda (&rest r)
                                        (declare (ignore r))
                                        (close-window ,var)
                                        (return-from ,block))))
           (restart-bind
               ((close-window (lambda () (signal 'close-window))
                  :report-function (lambda (s) (format s "Call CLOSE-WINDOW on ~S, continuing" ,var))))
             ,@body))))))

(defun focused-window () (window-from-id *focused-window-id*))
(defun last-window () (window-from-id *last-window-id*))

(defun main-loop-function (ev idle-p)
  "This is called every iteration of the main loop.  It exists
primarily so it can be easily redefined without starting/stopping."
  (plus-c:c-let ((event sdl2-ffi:sdl-event :from ev))
    ;; Note GET-EVENT-TYPE gives us :LISP-MESSAGE
    (let ((type (when ev (sdl2:get-event-type ev)))
          (*event* ev))
      (cond
        (idle-p
         (loop for id being each hash-key in *idle-render-windows*
               do (render (gethash id *all-windows*))))
        ((eq :lisp-message type)
         (sdl2::get-and-handle-messages))
        ((eq :controlleraxismotion type)
         (let ((window (focused-window)))
           (when window
             (controller-axis-motion-event
              window
              (gethash (event :cbutton :which)
                       *id-to-controller*)
              (event :caxis :timestamp)
              (event :caxis :axis)
              (event :caxis :value)))))
        ((or (eq :controllerbuttondown type)
             (eq :controllerbuttonup type))
         (let ((window (focused-window)))
           (when window
             (controller-button-event
              window
              (gethash (event :cbutton :which)
                       *id-to-controller*)
              type
              (event :cbutton :timestamp)
              (event :cbutton :button)))))
        ((or (eq :mousebuttondown type)
             (eq :mousebuttonup type))
         (let ((window (window-from-id (event :button :window-id))))
           (when window
             (mousebutton-event window
                                type
                                (event :button :timestamp)
                                (event :button :button)
                                (event :button :x)
                                (event :button :y)))))
        ((eq :mousemotion type)
         (let ((window (window-from-id (event :motion :window-id))))
           (when window
             (mousemotion-event window
                                (event :motion :timestamp)
                                (event :motion :state)
                                (event :motion :x)
                                (event :motion :y)
                                (event :motion :xrel)
                                (event :motion :yrel)))))
        ((or (eq :keydown type)
             (eq :keyup type))
         (let ((window (window-from-id (event :key :window-id))))
           (when window
             (keyboard-event window
                             type
                             (event :key :timestamp)
                             (/= 0 (event :key :repeat))
                             (event :key :keysym)))))
        ((eq :textinput type)
         (let ((window (window-from-id (event :text :window-id))))
           (when window
             (textinput-event window
                              (event :text :timestamp)
                              (cffi:foreign-string-to-lisp
                               (event :text :text plus-c:&))))))
        ((eq :mousewheel type)
         (let ((window (window-from-id (event :wheel :window-id))))
           (when window
             (mousewheel-event window
                               (event :wheel :timestamp)
                               (event :wheel :x)
                               (event :wheel :y)))))
        ((eq :windowevent type)
         (let ((window (window-from-id (event :window :window-id)))
               (window-event-type
                 (autowrap:enum-key 'sdl2-ffi:sdl-window-event-id
                                    (event :window :event))))
           (when window
             (case window-event-type
               (:focus-gained
                (setf *focused-window-id* (event :window :window-id))
                (setf *last-window-id* *focused-window-id*))
               (:focus-lost (setf *focused-window-id* nil)))
             (window-event window
                           window-event-type
                           (event :window :timestamp)
                           (event :window :data1)
                           (event :window :data2)))))
        ((eq :controllerdeviceadded type)
         (let* ((id (event :cdevice :which))
                (c (sdl2:game-controller-open id))
                (instance-id (sdl2:game-controller-instance-id c)))
           (setf (gethash instance-id *id-to-controller*) c)
           (for-all-windows (w)
             (controller-added-event w c))))
        ((eq :controllerdeviceremoved type)
         (let* ((instance-id (event :cdevice :which))
                (c (gethash instance-id *id-to-controller*)))
           (for-all-windows (w)
             (controller-removed-event w c))
           (sdl2:game-controller-close c)
           (remhash instance-id *id-to-controller*)))
        (t
         (let ((window (focused-window)))
           (when window
             (other-event window ev))))))))

(defun main-loop ()
  (let (*main-loop-quit*)
    (sdl2:with-sdl-event (ev)
      (loop as method = (if (> (hash-table-count *idle-render-windows*) 0)
                            :poll :wait)
            as rc = (sdl2:next-event ev method)
            as idle-p = (and (= 0 rc) (eq :poll method))
            do (handler-case
                   (if *main-loop-quit*
                       (return-from main-loop)
                       (main-loop-function (unless idle-p ev) idle-p))
                 (sdl2:sdl-continue (c) (declare (ignore c))))))))

(defun start (&optional function)
  (unless *started*
    (setf *started* t)
    (handler-case
        (unless (sdl2:was-init :everything)
          (sdl2:init :everything))
      (error () (setf *started* nil)))
    (sdl2:in-main-thread (:background t :no-event t)
      (unwind-protect
           (progn
             (when function (funcall function))
             (main-loop)
             (sdl2:quit))
        (setf *started* nil)))))

(defmacro with-start ((&key this-thread-p) &body body)
  (if this-thread-p
      `(sdl2:make-this-thread-main
        (lambda ()
          (kit.sdl2:start (lambda () ,@body))))
      `(kit.sdl2:start (lambda () ,@body))))

(defun quit ()
  (sdl2:in-main-thread ()
    (setf *main-loop-quit* t)))
