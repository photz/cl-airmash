(declaim (optimize (speed 0) (debug 3) (safety 3)))
(sb-ext:restrict-compiler-policy 'debug 3)

(in-package :airmash-client)


(defun send-message (client player-command)
  (let ((res (flexi-streams:with-output-to-sequence (out-raw)
               (with-wrapped-in-bit-stream (out out-raw :byte-order :big-eendian)
                 (write-binary player-command out)))))

    (wsd:send-binary client res)))

(defun on-message (client world raw-msg)

  (let ((msg nil))
    (flexi-streams:with-input-from-sequence (my-new-stream raw-msg)
      (with-wrapped-in-bit-stream (in my-new-stream :byte-order :big-endian)
        (setf msg (read-binary 'server-msg in))))

    (let ((body (slot-value msg 'body)))
      (process client world body))))


(defun on-error (err)
  (format t "error: ~a~%" err))

(defun on-open (client world)
  (format t "Connected~%")
  (send-message client (make-player-login-command
                        :name "marilyn"
                        :horizon-x (/ 1920 2)
                        :horizon-y (/ 1920 2)
                        :flag "GB")))


(defun on-close (&key code reason)
  (format t "Closed because '~A' (Code=~A)~%" reason code))

(defstruct trans x y rot speed-x speed-y accel-x accel-y)
(defstruct user name score up down left right)

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
  (let ((speed-x (trans-speed-x transform))
        (speed-y (trans-speed-y transform)))
    (incf (trans-x transform) (* (/ td 1000.0) speed-x))
    (incf (trans-x transform) (* (/ td 1000.0) speed-y))))

(defmacro d->r (deg)
  `(* 0.01745329252 ,deg))

(defun make-mat4 (&optional (val 0.0))
  (make-array '(4 4) :initial-contents
	      `((,val 0.0 0.0 0.0)
		(0.0 ,val 0.0 0.0)
		(0.0 0.0 ,val 0.0)
		(0.0 0.0 0.0 ,val))))

(defun make-vec3 (&optional (x 0.0) (y 0.0) (z 0.0))
  (make-array '(3) :initial-contents `(,x ,y ,z)))

(defun make-vec4 (&optional (x 0.0) (y 0.0) (z 0.0) (w 0.0))
  (make-array '(4) :initial-contents `(,x ,y ,z ,w)))

(defmacro vx (v)
  `(aref ,v 0))

(defmacro vy (v)
  `(aref ,v 1))

(defmacro vz (v)
  `(aref ,v 2))

(defmacro vw (v)
  `(aref ,v 3))

(defun dot (v u)
  (apply #'+ (map 'list #'* v u)))

(defun norm (v)
  (sqrt (dot v v)))

(defun normalize (v)
  (let ((n (norm v)))
    (if (= n 0.0)
        v
        (map 'vector (lambda (x) (/ x n)) v))))

(defun cross (u v)
  (make-vec3 
   ( - (* (vy u) (vz v)) (* (vz u) (vy v)))
   ( - (* (vz u) (vx v)) (* (vx u) (vz v)))
   ( - (* (vx u) (vy v)) (* (vy u) (vx v)))))

(defun vdiff (u v)
  (make-vec3 (- (vx u) (vx v))
	     (- (vy u) (vy v))
	     (- (vz u) (vz v))))

(defun vadd (u v)
  (make-vec3 (+ (vx u) (vx v))
	     (+ (vy u) (vy v))
	     (+ (vz u) (vz v))))

(defun vprod (k v)
  (make-vec3 (* k (vx v))
	     (* k (vy v))
	     (* k (vz v))))

(defun mvprod (m v)
  (let ((u (make-vec3 0)))
    (dotimes (i 3)
      (dotimes (j 3)
	(incf (aref u i) (* (aref v j) (aref m j i))))
      (incf (aref u i) (aref m 3 i)))
    u))

(defun mprod (m1 m2)
  (let ((m (make-mat4)))
    (dotimes (i 4)
      (dotimes (j 4)
	(let ((sum 0.0))
	  (dotimes (k 4)
	    (incf sum (* (aref m1 k i) (aref m2 j k))))
	  (setf (aref m j i) sum))))
    m))

(defun translation (&optional (x 0.0) (y 0.0) (z 0.0))
  (let ((m (make-mat4 1.0)))
    (setf (aref m 3 0) x)
    (setf (aref m 3 1) y)
    (setf (aref m 3 2) z)
    m))

(defun rotation (a v)
  "Angle A in radians"
  (let* ((c (cos a))
	 (s (sin a))
	 (axis (normalize v))
	 (temp (vprod (- 1.0 c)  axis))
	 (result (make-mat4 1.0)))
    (setf (aref result 0 0) (+ c (* (vx temp) (vx axis))))
    (setf (aref result 0 1) (+ (* (vx temp) (vy axis)) (* s (vz axis))))
    (setf (aref result 0 2) (- (* (vx temp) (vz axis)) (* s (vy axis))))
    (setf (aref result 1 0) (- (* (vy temp) (vx axis)) (* s (vz axis))))
    (setf (aref result 1 1) (+ c (* (vy temp) (vy axis))))
    (setf (aref result 1 2) (+ (* (vy temp) (vz axis)) (* s (vx axis))))
    (setf (aref result 2 0) (+ (* (vz temp) (vx axis)) (* s (vy axis))))
    (setf (aref result 2 1) (- (* (vz temp) (vy axis)) (* s (vx axis))))
    (setf (aref result 2 2) (+ c (* (vz temp) (vz axis))))
    result))

(defun perspective (fovy aspect znear zfar)
  (let* ((range (* (tan (d->r (/ fovy 2.0))) znear))
	 (left (* aspect (- range)))
	 (right (* aspect range))
	 (bottom (- range))
	 (top range)
	 (result (make-mat4 0.0)))
    (setf (aref result 0 0) (/ (* 2.0 znear) (- right left)))
    (setf (aref result 1 1) (/ (* 2.0 znear) (- top bottom)))
    (setf (aref result 2 2) (- (/ (+ zfar znear) (- zfar znear))))
    (setf (aref result 2 3) (- 1.0))
    (setf (aref result 3 2) (- (/ (* 2.0 zfar znear) (- zfar znear))))
    result))

(defun ortho (left right bottom top znear zfar)
  (let ((result (make-mat4 1.0)))
    (setf (aref result 0 0) (/ 2.0 (- right left)))
    (setf (aref result 1 1) (/ 2.0 (- top bottom)))
    (setf (aref result 2 2) (/ -2.0 (- zfar znear)))
    (setf (aref result 3 0) (/ (+ right left) (- left right)))
    (setf (aref result 3 1) (/ (+ top bottom) (- bottom top)))
    (setf (aref result 3 2) (/ (+ zfar znear) (- znear zfar)))
    result))

(defun look-at (eye center up)
  (let* ((f (normalize (vdiff center eye)))
	 (u (normalize up))
	 (s (normalize (cross f u)))
	 (result (make-mat4 1.0)))
    (setf u (cross s f))
    (setf (aref result 0 0) (vx s))
    (setf (aref result 1 0) (vy s))
    (setf (aref result 2 0) (vz s))
    (setf (aref result 0 1) (vx u))
    (setf (aref result 1 1) (vy u))
    (setf (aref result 2 1) (vz u))
    (setf (aref result 0 2) (- (vx f)))
    (setf (aref result 1 2) (- (vy f)))
    (setf (aref result 2 2) (- (vz f)))
    (setf (aref result 3 0) (- (dot s eye)))
    (setf (aref result 3 1) (- (dot u eye)))
    (setf (aref result 3 2) (dot f eye))
    result))
   


(defun link-program (shader-source fragment-source)
  (let ((vs (gl:create-shader :vertex-shader))
        (fs (gl:create-shader :fragment-shader))
        (pg (gl:create-program)))
    (gl:shader-source vs shader-source)
    (gl:compile-shader vs)
    ;;(print (gl:get-shader-info-log vs))
    (gl:shader-source fs fragment-source)
    (gl:compile-shader fs)
    ;;(print (gl:get-shader-info-log fs))
    (gl:attach-shader pg vs)
    (gl:attach-shader pg fs)
    (gl:link-program pg)
    ;;(print (gl:get-program-info-log pg))
    pg))

(defun load-buffer-array (array)
  (let* ((len (length array))
         (arr (gl:alloc-gl-array :float len)))
    (dotimes (i len)
      (setf (gl:glaref arr i) (aref array i)))
    (gl:buffer-data :array-buffer :static-draw arr)
    (gl:free-gl-array arr)))
 
(defun enable-vertex-array (index buffer)
  (gl:enable-vertex-attrib-array index)
  (gl:bind-buffer :array-buffer buffer)
  (gl:vertex-attrib-pointer index 3 :float nil 0 (cffi:null-pointer)))


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
   (ws-client :initarg :ws-client)))

(defun tutorial4 (world client)
  (glop:with-window (win "tutorial 4" 400 300 :major 3 :minor 3 :win-class 'airmash-window)
    (setf (slot-value win 'ws-client) client)
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


(let ((keyseq 0))
  (defun set-key (client key pressed)
    (check-type key keyword)
    (check-type pressed boolean)
    (let ((val (case key
                 (:up 1)
                 (:down 2)
                 (:left 3)
                 (:right 4))))
      (if val
          (let ()
            (send-message client (make-player-key-command :seq keyseq
                                                        :key val
                                                        :state (if pressed 1 0)))
            (incf keyseq))))))
        


(defmethod glop:on-event (window (event glop:key-event))
  (case (glop:keysym event)
    (:escape (glop:push-close-event window))
    (otherwise (set-key (slot-value window 'ws-client)
                        (glop:keysym event)
                        (glop:pressed event)))))

(defmethod glop:on-event (window (event glop:resize-event))
  (gl:viewport 0 0 (glop:width event) (glop:height event)))



(defun main ()
  (let* ((host "wss://game-eu-s1.airma.sh/ffa2")
         (test-host "ws://localhost:3000")
         (host-asia "wss://game-asia-s1.airma.sh/ffa1")
         (ws-server host-asia)
         (world (make-instance 'ecs:world))
         (client (wsd:make-client ws-server
                                  :additional-headers
                                  '(("Origin" . "https://airma.sh")))))

    (wsd:on :open client (alexandria:curry #'on-open client world))
    (wsd:on :message client (alexandria:curry #'on-message client world))
    (wsd:on :error client #'on-error)
    (wsd:on :close client #'on-close)

    (wsd:start-connection client)
    (tutorial4 world client)))

