(declaim (optimize (speed 0) (debug 3) (safety 3)))
(sb-ext:restrict-compiler-policy 'debug 3)

(in-package :airmash-client)


(defstruct trans
  (x 0.0 :type float)
  (y 0.0 :type float)
  (rot 0.0 :type float)
  (speed-x 0.0 :type float)
  (speed-y 0.0 :type float)
  (accel-x 0.0 :type float)
  (accel-y 0.0 :type float))

(defstruct user
  (name "" :type string)
  (score 0 :type integer)
  (up nil :type boolean)
  (down nil :type boolean)
  (left nil :type boolean)
  (right nil :type boolean))




(defun print-table (e-id td user transform)
  (check-type td number)
  (format t "~a is at ~a,~a and his score is ~a (~a)~%"
          (user-name user)
          (trans-x transform)
          (trans-y transform)
          (user-score user)
          (trans-rot transform)))

(defun update-trans (e-id td transform)
  (check-type td number)
(defparameter *tutorial4-shader-source* 
"#version 330 core
layout(location = 0) in vec3 in_Position;
layout(location = 1) in vec3 in_Color;
uniform mat4 MVP;
out vec3 ex_Color;
void main() {
  gl_Position = MVP * vec4(in_Position, 1.0);
  ex_Color = in_Color;
}")

(defparameter *tutorial4-fragment-source*
"#version 330 core
in vec3 ex_Color;
out vec3 out_Color;
void main() {
  out_Color = ex_Color;
}")

(defclass airmash-window (glop:window)
  ((zoom :initform 1)
   (world :initarg :world)
   (client :initarg :client)))

(defun run (world client)
  (glop:with-window (win "tutorial 4" 400 300 :major 3 :minor 3 :win-class 'airmash-window)
    (setf (slot-value win 'client) client)
    (setf (slot-value win 'world) world)
    (let* ((vertex-array (gl:gen-vertex-array))
	   (buffers (gl:gen-buffers 2))
	   (vertex-buffer (elt buffers 0))
	   (color-buffer (elt buffers 1))
	   (program (link-program *tutorial4-shader-source* *tutorial4-fragment-source*))
	   (matrixId (gl:get-uniform-location program "MVP"))
	   (projection (perspective 45.0 (/ 4.0 3.0) 0.1 300.0))
	   (camera (make-vec3 4 10 3))
	   (model (make-mat4 1.0))
	   (view (look-at camera #(0 0 0) #(0 1 0)))
	   MVP
           (last-update-trans (get-internal-real-time))
	   (angle 0.0))
      (gl:bind-vertex-array vertex-array)
      (gl:bind-buffer :array-buffer vertex-buffer)
      (load-buffer-array #(0.1 0.0 0.1
                           0.0 0.0 -.1
                           -0.1 0.0 0.1
			   ))
      (gl:bind-buffer :array-buffer color-buffer)
      (load-buffer-array #(1.0 0.0 0.0
			   0.0 1.0 0.0
			   0.0 0.0 1.0
			   ))
      (gl:clear-color 0 0 0 0)
      (gl:enable :depth-test :cull-face)
      (gl:depth-func :less)
      (loop while (glop:dispatch-events win :blocking nil :on-foo nil) do
	   (setf model (rotation 0 #(0 0 1)))
	   (setf view (look-at 
		       (mvprod (rotation (d->r angle) #(0 1 0)) camera)
		       #(0 0 0) 
		       #(0 1 0)))
	   (setf MVP (mprod projection (mprod view model)))
	   (gl:clear :color-buffer-bit :depth-buffer-bit)
	   (gl:use-program program)
	   (gl:uniform-matrix matrixId 4 `#(,MVP) nil)
	   (enable-vertex-array 0 vertex-buffer)
	   (enable-vertex-array 1 color-buffer)
           ;;(gl:draw-arrays :triangles 0 3)

           (ecs:update-components
            world
            1
            #'(lambda (entity-id td transform)
                (let* ((pos-x (trans-x transform))
                       (pos-y (trans-y transform))
                       (scale 1000.0)
                       (mapped-x (/ pos-x scale))
                       (mapped-y (/ pos-y scale))
                       (the-rot (rotation (trans-rot transform) #(0 1 0)))
                       (the-transl (translation mapped-x 0.01 mapped-y))
                       (modelmat (mprod the-transl the-rot))
                       
                       (mvp2 (mprod projection (mprod view modelmat))))
                  (gl:uniform-matrix matrixId 4 `#(,mvp2) nil)
                  (gl:draw-arrays :triangles 0 3)))

            :transforms)

           (ecs:update-components world (- (get-internal-real-time) last-update-trans) #'update-trans :transforms)
           (setf last-update-trans (get-internal-real-time))
           ;;(ecs:update-components world #'print-table :users :transforms)
	   (gl:disable-vertex-attrib-array 0)
	   (gl:disable-vertex-attrib-array 1)
	   (glop:swap-buffers win)
	   (incf angle 0.3))
      (gl:delete-buffers `(,vertex-buffer ,color-buffer))
      (gl:delete-program program)
      (gl:delete-vertex-arrays `(,vertex-array)))))



(defun set-key (user-comp client key pressed)
  (check-type key keyword)
  (check-type pressed boolean)
  (let ((val (case key
               (:up 'up)
               (:down 'down)
               (:left 'left)
               (:right 'right))))
    (when val
      (send-key client key pressed)
      (setf (slot-value user-comp val) pressed))))

        


(defmethod glop:on-event (window (event glop:key-event))
  (case (glop:keysym event)
    (:escape (glop:push-close-event window))
    (otherwise (set-key (ecs:get-component (slot-value window 'world)
                                           (slot-value (slot-value window 'client) 'player-id)
                                           :users)
                        (slot-value window 'client)
                        (glop:keysym event)
                        (glop:pressed event)))))


(defmethod glop:on-event (window (event glop:resize-event))
  (gl:viewport 0 0 (glop:width event) (glop:height event)))



(defun main ()
  (let* ((host "wss://game-eu-s1.airma.sh/ffa2")
         (test-host "ws://localhost:3000")
         (host-asia "wss://game-asia-s1.airma.sh/ffa1")
         (world (make-instance 'ecs:world))
         (client (make-instance 'airmash-client
                                :host host-asia
                                :on-message (alexandria:rcurry #'process world)
                                :player-name "johndoe"
                                :player-flag "gb")))
                                                                     


    (run world client)))
