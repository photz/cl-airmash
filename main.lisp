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


(defclass rendering-system-class ()
  ((program :initform nil)
   (vertex-buffer :initform nil)
   (color-buffer :initform nil)))
  

(defmethod initialize-instance :after ((s rendering-system-class) &key)
  (gl:enable :depth-test :cull-face)
  (gl:depth-func :less)
  (gl:clear-color 0 0 0 0)

  (let ((buffers (gl:gen-buffers 2))
        (vertex-array (gl:gen-vertex-array))
        (vshader (3bgl-shaders:generate-stage :vertex 'shaders::vertex :version 330))
        (fshader (3bgl-shaders:generate-stage :fragment 'shaders::fragment :version 330)))

    (print vshader)
    (print fshader)

    (setf (slot-value s 'program)
          (opengl-helper:link-program vshader fshader))

    (setf (slot-value s 'vertex-buffer)
          (elt buffers 0))

    (setf (slot-value s 'color-buffer)
          (elt buffers 1))

    (gl:bind-vertex-array vertex-array)
    (gl:bind-buffer :array-buffer (slot-value s 'vertex-buffer))
    (opengl-helper:load-buffer-array #(0.1 0.0 0.1
                                       0.0 0.0 -.1
                                       -0.1 0.0 0.1))


    (gl:bind-buffer :array-buffer (slot-value s 'color-buffer))
    (opengl-helper:load-buffer-array #(1.0 0.0 0.0
                                       0.0 1.0 0.0
                                       0.0 0.0 1.0))))

(defmethod run-system ((s rendering-system-class) world)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:use-program (slot-value s 'program))
  (let* ((camera (make-vec3 4 10 3))
         (angle 0.0)
         (view (look-at (mvprod (rotation (d->r angle) #(0 1 0)) camera)
                        #(0 0 0) 
                        #(0 1 0)))
         (projection (perspective 45.0 (/ 4.0 3.0) 0.1 300.0))
         (program (slot-value s 'program)))


    (opengl-helper:enable-vertex-array 0 (slot-value s 'vertex-buffer))
    (opengl-helper:enable-vertex-array 1 (slot-value s 'color-buffer))
    
    (ecs:update-components
     world
     1
     #'(lambda (entity-id td transform)
         (let* ((pos-x (trans-x transform))
                (pos-y (trans-y transform))
                (scale 1000.0)
                (mapped-x (/ pos-x scale))
                (mapped-y (/ pos-y scale))
                (the-rot (rotation (- (trans-rot transform)) #(0 1 0)))
                (the-transl (translation mapped-x 0.01 mapped-y))
                (modelmat (mprod the-transl the-rot))
                
                (mvp2 (mprod projection (mprod view modelmat))))

           (gl:uniform-matrix (gl:get-uniform-location program "mvp")
                              4 `#(,mvp2) nil)

           (gl:draw-arrays :triangles 0 3)))

     :transforms)

    (gl:disable-vertex-attrib-array 0)
    (gl:disable-vertex-attrib-array 1)))



    

(defun print-table (e-id td user transform)
  (check-type td number)
  (format t "~a is at ~a,~a and his score is ~a (~a)~%"
          (user-name user)
          (trans-x transform)
          (trans-y transform)
          (user-score user)
          (trans-rot transform)))

(defun angle-to-vec (angle)
  (let* ((x (sin angle))
         (y (- (cos angle)))
         (mag (sqrt (+ (expt x 2) (expt y 2)))))
    (values (/ x mag) (/ y mag))))
         
           
(defun update-trans (e-id td user transform)
  (check-type td number)
  (check-type (user-up user) boolean)
  (check-type (user-down user) boolean)
  (let* ((vel 330.0))

    (multiple-value-bind (x y) (angle-to-vec (trans-rot transform))
      (when (or (user-up user) (user-down user))
        (incf (trans-x transform)
              (* (/ td 1000.0) vel x))

        (incf (trans-y transform)
              (* (/ td 1000.0) vel y)))))

  (let ((rot-factor 0)
        (crnt-rot (trans-rot transform)))

    (when (user-left user)
      (decf rot-factor))

    (when (user-right user)
      (incf rot-factor))

    (when (/= 0 rot-factor)
      (setf (trans-rot transform)
            (mod (+ crnt-rot (* rot-factor (/ td 1000.0) 3.0)) pi)))))


(defun set-key (user-comp client key pressed)
  (check-type key keyword)
  (check-type pressed boolean)
  (let ((val (case key
               (:up 'up)
               (:down 'down)
               (:left 'left)
               (:right 'right))))
    (if (eq :space key)
        (send-key client key pressed))
    (when val
      (send-key client key pressed)
      (setf (slot-value user-comp val) pressed))))

(defun on-key (window event world client)
  (case (glop:keysym event)
    (:escape (glop:push-close-event window))
    (otherwise (set-key (ecs:get-component world
                                           (slot-value client 'player-id)
                                           :users)
                        client
                        (glop:keysym event)
                        (glop:pressed event)))))


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

    (glop:with-window (win "Airmash" 400 300 :major 3 :minor 3)
      (loop
         with rendering-sys = (make-instance 'rendering-system-class)
         with last-update-trans = (get-internal-real-time)
         for evt = (glop:next-event win :blocking nil)
         with running = t
         while running
         if evt
         do (typecase evt
              (glop:key-event (on-key win evt world client))
              (glop:close-event (setf running nil))
              (glop:resize-event (gl:viewport 0 0 (glop:width evt) (glop:height evt)))
              (t (format t "Unhandled event: ~A~%" evt)))
         else
         do (ecs:update-components world (- (get-internal-real-time) last-update-trans) #'update-trans :users :transforms)
           (setf last-update-trans (get-internal-real-time))
           (run-system rendering-sys world)
           (glop:swap-buffers win)))))

;; (gl:delete-buffers `(,vertex-buffer ,color-buffer))
;; (gl:delete-program program)
;; (gl:delete-vertex-arrays `(,vertex-array)))))

