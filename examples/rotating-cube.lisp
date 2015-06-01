(in-package :kit.sdl2.test)

;;; This requires your graphics card to support GL 3.3 / GLSL 3.30!

;;; Renders a rotating cube to the screen which can be moved with the mouse by holding
;;; down the left mouse button. Also you may zoom in and out with the mousewheel.


;;; HOW TO USE:
;;;
;;; First, run this. It is SAFE to run repeatedly:
;;;
;;; (sdl2.kit:start)
;;;
;;; Then, make a window.
;;;
;;;   (make-instance 'sdl2.kit.test:cube-window)
;;;
;;; After you close a window, it will be collected at some point.

;;; You should NOT call any protocol functions on a window, except the
;;; following:
;;;
;;;   (render WINDOW)
;;;   (close-window WINDOW)
;;;
;;; These are the only functions guaranteed to be "safe" (including
;;; threadsafety and other expectations).


(defclass cube-window (gl-window)
  ((start-time :initform (get-internal-real-time))
   (one-frame-time :initform (get-internal-real-time))
   (frames :initform 0)))


;;Data:--------------------------------------------------------------------------

(defvar *cube-positions*
  (cffi:foreign-alloc
   :float
   :initial-contents
   '(0.5 0.5 0.5			;0
     0.5 -0.5 0.5			;1
     -0.5 -0.5 0.5			;2
     -0.5 0.5 0.5			;3
     0.5 0.5 -0.5			;4
     0.5 -0.5 -0.5			;5
     -0.5 -0.5 -0.5			;6
     -0.5 0.5 -0.5)))			;7

(defvar *cube-colors*
  (cffi:foreign-alloc
   :float
   :initial-contents
   '(0.2 0.2 0.2			;0
     0.3 0.3 0.3			;1
     0.4 0.4 0.4			;2
     0.5 0.5 0.5			;3
     0.6 0.6 0.6			;4
     0.7 0.7 0.7			;5
     0.8 0.8 0.8			;6
     0.9 0.9 0.9)))			;7

(defvar *cube-indices*
  (cffi:foreign-alloc
   :unsigned-short
   :initial-contents
   '(;; front
     0 1 2
     2 3 0
     ;; right-side
     4 5 1
     1 0 4
     ;; back
     7 6 5
     5 4 7
     ;; left-side
     3 2 6
     6 7 3
     ;; top
     4 0 3
     3 7 4
     ;; bottom
     1 5 6
     6 2 1)))


;;Shader------------------------------------------------------------------------

;; the returned dictionary with the programs can be used like so:
;; (1) get the program directly (find-program <compiled-dictionary> <program-name>)
;; (2) or just use it directly (use-program <compiled-dictionary> <program-name>)
;;     also (use-program 0) works
(defun load-shaders ()
  (defdict shaders (:shader-path
                    (merge-pathnames
                     #p "examples/shaders/" (asdf/system:system-source-directory :sdl2kit-examples)))
    ;; instead of (:file <path>) you may directly provide the shader as a string containing the
    ;; source code
    (shader matrix-perspective-v :vertex-shader (:file "transform-and-project.vert"))
    (shader color-pass-through-f :fragment-shader (:file "color-pass-through.frag"))
    ;; here we compose the shaders into programs, in this case just one ":basic-projection"
    (program :basic-projection (:model-to-clip :perspective-matrix) ;<- UNIFORMS!
             (:vertex-shader matrix-perspective-v)
             (:fragment-shader color-pass-through-f)))
  ;; function may only run when a gl-context exists, as its documentation
  ;; mentions
  (compile-shader-dictionary 'shaders))

(defvar *programs-dict*)

(defun initialize-program ()
  (setf *programs-dict* (load-shaders)))

;; to be understood while reading the LOAD-SHADER function
;; example: (uniform :vec :<name-of-uniform> <new-value>)
(defgeneric uniform (type key value)
  (:method ((type (eql :vec)) key value)
    (uniformfv *programs-dict* key value))

  (:method ((type (eql :vec)) key value)
    (uniformfv *programs-dict* key value))

  (:method ((type (eql :mat)) key value)
    ;; nice, transpose is NIL by default!
    (uniform-matrix *programs-dict* key 4 value NIL)))


;;VAO setup.....................................................................

(defvar *vao* 0)

(defun initialize-vao ()
  (let ((vao (first (gl:gen-vertex-arrays 1)))
        (vbo (first (gl:gen-buffers 1)))
        (ibo (first (gl:gen-buffers 1))))
    (gl:bind-vertex-array vao)
    ;;VBO
    (gl:bind-buffer :array-buffer vbo)
    ;;VBO - positions
    ;;to avoid magic numbers (* 24 4 3 2) the gl:gl-array (gl:alloc-gl-array) can be used,
    ;;which is a struct containing field with a pointer to the forein-memory and a field
    ;;with the size.  In this case ommited for the sake of a terse example.
    ;; Layout:
    ;; 24 number of vertices/colors
    ;; 4 size of the float data type
    ;; 3 x,y,z coordinate (positions) or red,green,blue (colors)
    ;; 2 array contains 24 position vertices and, again, 24 color vertices
    (%gl:buffer-data :array-buffer (* 24 4 3 2) *cube-positions* :static-draw)
    (%gl:enable-vertex-attrib-array 0)
    (%gl:vertex-attrib-pointer 0 3 :float :false 0 0)
    ;;VBO - colors
    ;; color sub-data starts in vbo exactly after the position vertices hence
    ;; the offset (* 24 4 3) and its size is also (* 24 4 3) as every vertices
    ;; has its own color
    (%gl:buffer-sub-data :array-buffer (* 24 4 3) (* 24 4 3) *cube-colors*)
    (%gl:enable-vertex-attrib-array 1)
    (%gl:vertex-attrib-pointer 1 3 :float :false 0 0)

    ;;IBO
    (gl:bind-buffer :element-array-buffer ibo)
    ;; why (* 36 2)?
    ;; it takes 2 triangles to draw the side of cube, hence to draw a whole cube:
    ;; (* 2 6) => 12. Each triangle consists of 3 vertices, hence, (* 3 12) => 36
    ;; and the index buffer's indices first point to the vertices in the vbo,
    ;; supplied by *cube-positions*, and then for each position to the corresponding
    ;; color in the vbo, supplied by *cube-colors*, hence 36 times 2!
    (%gl:buffer-data :element-array-buffer (* 36 2) *cube-indices* :static-draw)

    (gl:bind-vertex-array 0)
    (setf *vao* vao)))

;;utils-------------------------------------------------------------------------

(defun framelimit (window &optional (fps 60))
  "Issues SDL2:DELAY's to get desired FPS."
  (with-slots (one-frame-time) window
    (let ((elapsed-time (- (get-internal-real-time) one-frame-time))
          (time-per-frame (/ 1000.0 fps)))
      (when (< elapsed-time time-per-frame)
        (sdl2:delay (floor (- time-per-frame elapsed-time))))
      (setf one-frame-time (get-internal-real-time)))))


(defun display-fps (window)
  (with-slots (start-time frames) window
    (incf frames)
    (let* ((current-time (get-internal-real-time))
           (seconds (/ (- current-time start-time) internal-time-units-per-second)))
      (when (> seconds 5)
        (format t "FPS: ~A~%" (float (/ frames seconds)))
        (setf frames 0)
        (setf start-time (get-internal-real-time))))))


;;init code---------------------------------------------------------------------

(defmethod initialize-instance :after ((w cube-window) &key &allow-other-keys)
  ;; GL setup can go here; your GL context is automatically active,
  ;; and this is done in the main thread.

  ;; if you (setf (idle-render window) t) it'll call RENDER as fast as
  ;; possible when not processing other events - suitable for games
  (setf (idle-render w) t)
  (gl:clear-color 0 0 1 1)
  (gl:clear :color-buffer-bit)
  (gl:viewport 0 0 800 600)

  ;; with culling
  (gl:enable :cull-face)
  (gl:cull-face :back)
  (gl:front-face :cw)

  (initialize-program)
  (initialize-vao))

;;Rendering----------------------------------------------------------------------

(defparameter *rotate-x* 1.0)
(defparameter *rotate-y* 0.0)
(defparameter *zoom-z* -2.0)

(defun draw-cube ()
  (gl:bind-vertex-array *vao*)
  (use-program *programs-dict* :basic-projection)
  ;; all the neat transformations take place here
  (uniform :mat :model-to-clip
           (vector
            (sb-cga:matrix*
             (sb-cga:translate (vec3 0.0 0.0 *zoom-z*))
             (sb-cga:rotate (vec3 *rotate-x* *rotate-y* 0.0))
             (sb-cga:rotate (vec3 0.0 (mod (/ (sdl2:get-ticks) 5000.0) (* 2 3.14159)) 0.0)))))
  ;; projection matrix
  (uniform :mat :perspective-matrix
           (vector (perspective-matrix (* pi 1/3) 1/1 0.0 1000.0)))

  (%gl:draw-elements :triangles (* 36 2) :unsigned-short 0)
  (gl:bind-vertex-array 0))


(defmethod render ((window cube-window))
  ;; Your GL context is automatically active.  FLUSH and
  ;; SDL2:GL-SWAP-WINDOW are done implicitly by GL-WINDOW  (!!)
  ;; after RENDER.
  (gl:clear :color-buffer)

  (draw-cube)

  (display-fps window)
  (framelimit window 60))

;;Events------------------------------------------------------------------------

(defmethod close-window ((window cube-window))
  (format t "Bye!~%")
  ;; To _actually_ destroy the GL context and close the window,
  ;; CALL-NEXT-METHOD.  You _may_ not want to do this, if you wish to
  ;; prompt the user!
  (call-next-method))

(defmethod mousewheel-event ((window cube-window) ts x y)
  ;; zoom in/out
  (cond ((= y 1) (incf *zoom-z* 0.2))
        ((= y -1) (decf *zoom-z* 0.2)))
  (render window))


(defmethod keyboard-event ((window cube-window) state ts repeat-p keysym)
  (let ((scancode (sdl2:scancode keysym)))
    (when (eq :scancode-escape scancode)
      (close-window window))))

(defmethod mousebutton-event ((window cube-window) state ts b x y)
  (format t "~A button: ~A at ~A, ~A~%" state b x y))

(defmethod mousemotion-event ((window cube-window) ts mask x y xr yr)
  (flet ((left-mouse-button-clicked-p ()
           (= mask 1)))
    ;; rotate x, y axis
    (when (left-mouse-button-clicked-p)
      (incf *rotate-y* (/ xr 100.0))
      (incf *rotate-x* (/ yr 100.0)))))
