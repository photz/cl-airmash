(declaim (optimize (speed 0) (debug 3) (safety 3)))
(sb-ext:restrict-compiler-policy 'debug 3)

(in-package :airmash-client)


(defun send-message (client player-command)
  (let ((res (flexi-streams:with-output-to-sequence (out-raw)
               (with-wrapped-in-bit-stream (out out-raw :byte-order :big-eendian)
                 (write-binary player-command out)))))

    (wsd:send-binary client res)))

(defmethod process (client world (body server-error-msg))
  (format t "Error code: ~a~%" (slot-value body 'err)))

(defmethod process (client world (body server-ping-msg))
  (let* ((ping-num (slot-value body 'num))
         (pong (make-player-pong-command :num ping-num)))
    (send-message client pong)))

(defmethod process (client world (body server-ping-result-msg))
  (format t "Got a ping of ~a~%" (slot-value body 'ping)))  

(defmethod process (client world (body server-player-update-msg))
  (let* ((user-id (slot-value body 'id))
         (pos-x (slot-value body 'pos-x))
         (pos-y (slot-value body 'pos-y))
         (rot (slot-value body 'rotation))

         (trans (ecs:get-component world user-id :transforms)))

    (setf (trans-x trans) pos-x)
    (setf (trans-y trans) pos-y)
    (setf (trans-rot trans) rot)))



(defmethod process (client world (body server-player-hit-msg)))
;;(send-message client (make-player-say-command
;;:text "ouch")))


(defmethod process (client world (body server-login-msg))
  (format t "Server confirmed login.~%")
  (let ((users (slot-value body 'players)))

    (loop
       for user being the elements of users
       for user-id = (slot-value user 'id)
       for name = (slot-value user 'name)
       for pos-x = (slot-value user 'pos-x)
       for pos-y = (slot-value user 'pos-y)
       for rot = (slot-value user 'rotation)
       do (let ()
            (ecs:add-entity world user-id)
            (ecs:set-component world user-id :transforms
                               (make-trans :x pos-x
                                           :y pos-y
                                           :rot rot))
            (ecs:set-component world user-id :users
                               (make-user :name name
                                          :score 0))))))


(defmethod process (client world (body server-score-update-msg)))

(defmethod process (client world (body server-player-leave-msg))
  (let ((user-id (slot-value body 'id)))
    (ecs:remove-entity world user-id)))

(defmethod process (client world (body server-player-new-msg))
  (let ((player-id (slot-value body 'id))
        (player-name (slot-value body 'name))
        (pos-x (slot-value body 'pos-x))
        (pos-y (slot-value body 'pos-y))
        (rot (slot-value body 'rot)))

    (ecs:add-entity world player-id)
    (ecs:set-component world player-id :transforms
                       (make-trans :x pos-x
                                   :y pos-y
                                   :rot rot))
    (ecs:set-component world player-id :users
                       (make-user :name player-name
                                  :score 0))))



(defmethod process (client world (body t)))
  ;;(format t "no handler for ~a~%" body))



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

(defstruct trans x y rot)
(defstruct user name score)

(defun print-table (e-id td user transform)
  (format t "~a is at ~a,~a and his score is ~a~%"
          (user-name user)
          (user-score user)
          (trans-x transform)
          (trans-y transform)))

(defun main ()
  (let* ((host "wss://game-eu-s1.airma.sh/ffa1")
         (test-host "ws://localhost:3000")
         (ws-server test-host)
         (world (make-instance 'ecs:world))
         (client (wsd:make-client ws-server
                                  :additional-headers
                                  '(("Origin" . "https://airma.sh")))))

    (wsd:on :open client (alexandria:curry #'on-open client world))
    (wsd:on :message client (alexandria:curry #'on-message client world))
    (wsd:on :error client #'on-error)
    (wsd:on :close client #'on-close)

    (wsd:start-connection client)

    (loop
       do (let ((tick 3))
            (sleep tick)
            (ecs:update-components world #'print-table :users :transforms)))

    (format t "Exiting.~%")))
