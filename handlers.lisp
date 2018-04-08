(in-package :airmash-client)

(defmethod process (client (body server-error-msg) world)
  (format t "Error code: ~a~%" (slot-value body 'err)))

(defmethod process (client (body server-ping-msg) world)
  (let* ((ping-num (slot-value body 'num))
         (pong (make-player-pong-command :num ping-num)))
    (send-message client pong)))

(defmethod process (client (body server-ping-result-msg) world)
  (format t "Got a ping of ~a~%" (slot-value body 'ping)))  


(defmethod process (client (body server-player-update-msg) world)
  (let* ((user-id (slot-value body 'id))
         (pos-x (slot-value body 'pos-x))
         (pos-y (slot-value body 'pos-y))
         (speed-x (slot-value body 'speed-x))
         (speed-y (slot-value body 'speed-y))
         (rot (slot-value body 'rotation))
         (trans (ecs:get-component world user-id :transforms))
         (user-comp (ecs:get-component world user-id :users))
         (new-keystate (slot-value body 'keystate)))

    (setf (trans-x trans) pos-x
          (trans-y trans) pos-y
          (trans-speed-x trans) speed-x
          (trans-speed-y trans) speed-y
          (trans-rot trans) rot

          (user-up user-comp) (flag-to-bool (slot-value new-keystate 'up))
          (user-down user-comp) (flag-to-bool (slot-value new-keystate 'down))
          (user-left user-comp) (flag-to-bool (slot-value new-keystate 'left))
          (user-right user-comp) (flag-to-bool (slot-value new-keystate 'right)))))



(defmethod process (client (body server-player-hit-msg) world))
;;(send-message client (make-player-say-command
;;:text "ouch")))

(defmethod process (client (body server-event-repel-msg) world)
  (let ((users (slot-value body 'players)))
    (loop
       for user being the elements of users
       for user-id = (slot-value user 'id)
       for pos-x = (slot-value user 'pos-x)
       for pos-y = (slot-value user 'pos-y)
       for speed-x = (slot-value user 'speed-x)
       for speed-y = (slot-value user 'speed-y)
       for rot = (slot-value user 'rot)
       for trans-comp = (ecs:get-component world user-id :transforms)
       for user-comp = (ecs:get-component world user-id :users)
       for new-keystate = (slot-value user 'keystate)
       do (setf (trans-x trans-comp) pos-x
                (trans-y trans-comp) pos-y
                (trans-speed-x trans-comp) speed-x
                (trans-speed-y trans-comp) speed-y
                (trans-rot trans-comp) rot

                ;; Apply the new keystate
                (user-up user-comp) (flag-to-bool (slot-value new-keystate 'up))
                (user-down user-comp) (flag-to-bool (slot-value new-keystate 'down))
                (user-left user-comp) (flag-to-bool (slot-value new-keystate 'left))
                (user-right user-comp) (flag-to-bool (slot-value new-keystate 'right))))))

(defmethod flag-to-bool (flag)
  (if (= flag 1)
      t
      nil))
      

(defmethod process (client (body server-event-bounce-msg) world)
  (let* ((user-id (slot-value body 'id))
         (new-keystate (slot-value body 'keystate))
         (user-comp (ecs:get-component world user-id :users))
         (trans-comp (ecs:get-component world user-id :transforms)))

    (setf (trans-x trans-comp) (slot-value body 'pos-x)
          (trans-y trans-comp) (slot-value body 'pos-y)
          (trans-speed-x trans-comp) (slot-value body 'speed-x)
          (trans-speed-y trans-comp) (slot-value body 'speed-y)
          (trans-rot trans-comp) (slot-value body 'rotation)

          ;; Apply the new keystate
          (user-up user-comp) (flag-to-bool (slot-value new-keystate 'up))
          (user-down user-comp) (flag-to-bool (slot-value new-keystate 'down))
          (user-left user-comp) (flag-to-bool (slot-value new-keystate 'left))
          (user-right user-comp) (flag-to-bool (slot-value new-keystate 'right)))))


(defmethod process (client (body server-event-boost-msg) world)
  (let* ((user-id (slot-value body 'id))
         (pos-x (slot-value body 'pos-x))
         (pos-y (slot-value body 'pos-y))
         (speed-x (slot-value body 'speed-x))
         (speed-y (slot-value body 'speed-y))
         (trans-comp (ecs:get-component world user-id :transforms)))

    (setf (trans-x trans-comp) pos-x
          (trans-y trans-comp) pos-y
          (trans-speed-x trans-comp) speed-x
          (trans-speed-y trans-comp) speed-y)))
    


(defmethod process (client (body server-login-msg) world)
  (format t "Server confirmed login.~%")
  (let ((users (slot-value body 'players)))

    (setf *my-user-id* (slot-value body 'id))

    (loop
       for user being the elements of users
       for user-id = (slot-value user 'id)
       for name = (slot-value user 'name)
       for pos-x = (slot-value user 'pos-x)
       for pos-y = (slot-value user 'pos-y)
       for rot = (slot-value user 'rotation)
       do (progn
            (ecs:add-entity world user-id)
            (ecs:set-component world user-id :transforms
                               (make-trans :x pos-x
                                           :y pos-y
                                           :speed-x 0.0
                                           :speed-y 0.0
                                           :rot rot))
            (ecs:set-component world user-id :users
                               (make-user :name name
                                          :score 0))))))


(defmethod process (client (body server-score-update-msg) world)
  (let* ((user-id (slot-value body 'id))
         (new-score (slot-value body 'score))
         (user (ecs:get-component world user-id :users)))

    (format t "new score: ~a~%" new-score)
    (setf (user-score user) new-score)))


(defmethod process (client (body server-player-leave-msg) world)
  (let ((user-id (slot-value body 'id)))
    (ecs:remove-entity world user-id)))

(defmethod process (client (body server-player-new-msg) world)
  (let ((player-id (slot-value body 'id))
        (player-name (slot-value body 'name))
        (pos-x (slot-value body 'pos-x))
        (pos-y (slot-value body 'pos-y))
        (rot (slot-value body 'rot)))

    (ecs:add-entity world player-id)
    (ecs:set-component world player-id :transforms
                       (make-trans :x pos-x
                                   :y pos-y
                                   :speed-x 0.0
                                   :speed-y 0.0
                                   :rot rot))
    (ecs:set-component world player-id :users
                       (make-user :name player-name
                                  :score 0))))

(defmethod process (client (body server-player-respawn-msg) world)
  (let* ((user-id (slot-value body 'id))
         (pos-x (slot-value body 'pos-x))
         (pos-y (slot-value body 'pos-y))
         (rot (slot-value body 'rot))
         (user (ecs:get-component world user-id :transforms)))

    (setf (trans-x user) pos-x)
    (setf (trans-y user) pos-y)
    (setf (trans-rot user) rot)))




(defmethod process (client (body t) world))
  ;;(format t "no handler for ~a~%" body))


