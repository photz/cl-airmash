(declaim (optimize (speed 0) (debug 3) (safety 3)))
(sb-ext:restrict-compiler-policy 'debug 3)

(in-package :airmash-client)


(defun read-stream-content-into-byte-vector (stream &key ((%length length))
                                                         (initial-size 4096))
  "Return \"content\" of STREAM as freshly allocated (unsigned-byte 8) vector."
  ;;(check-type length (or null non-negative-integer))
  ;;(check-type initial-size positive-integer)
  (do ((buffer (make-array (or length initial-size)
                           :element-type '(unsigned-byte 8)))
       (offset 0)
       (offset-wanted 0))
      ((or (/= offset-wanted offset)
           (and length (>= offset length)))
       (if (= offset (length buffer))
           buffer
           (subseq buffer 0 offset)))
    (unless (zerop offset)
      (let ((new-buffer (make-array (* 2 (length buffer))
                                    :element-type '(unsigned-byte 8))))
        (replace new-buffer buffer)
        (setf buffer new-buffer)))
    (setf offset-wanted (length buffer)
          offset (read-sequence buffer stream :start offset))))



(defbinary player-login-command ()
  (command 0 :type (unsigned-byte 8))
  (protocol 5 :type (unsigned-byte 8))
  (name "" :type (counted-string 1 :external-format :utf8))
  (session "none" :type (counted-string 1 :external-format :utf8))
  (horizon-x 0 :type (unsigned-byte 16))
  (horizon-y 0 :type (unsigned-byte 16))
  (flag "" :type (counted-string 1 :external-format :utf8)))

(defbinary player-pong-command ()
  (command 6 :type (unsigned-byte 8))
  (num 0 :type (unsigned-byte 32)))

(defbinary player-chat-command ()
  (command 20 :type (unsigned-byte 8))
  (text "" :type (counted-string 1 :external-format :utf8)))

(defbinary player-ack-command ()
  (command 5 :type (unsigned-byte 8)))


;; UP 1
;; DOWN 2
;; LEFT 3
;; RIGHT 4
;; FIRE 5
;; SPECIAL 6
(defbinary player-key-command ()
  (command 10 :type (unsigned-byte 8))
  (seq 0 :type (unsigned-byte 32))
  (key 0 :type (unsigned-byte 8))
  (state 0 :type (unsigned-byte 8)))

(defbinary player-say-command ()
  (command 22 :type (unsigned-byte 8))
  (text "" :type (counted-string 1 :external-format :utf8)))

(defbinary player ()
  (id 0 :type (unsigned-byte 16))
  (status 0 :type (unsigned-byte 8))
  (level 0 :type (unsigned-byte 8))
  (name "" :type (counted-string 1 :external-format :utf8))
  (ship-type 0 :type (unsigned-byte 8))
  (team 0 :type (unsigned-byte 16))
  (pos-x 0 :type (unsigned-byte 16))
  (pos-y 0 :type (unsigned-byte 16))
  (rotation 0 :type (unsigned-byte 16))
  (flag 0 :type (unsigned-byte 16))
  (upgrades 0 :type (unsigned-byte 8)))


(defbinary server-login-msg ()
  (success 0 :type (unsigned-byte 8))
  (id 0 :type (unsigned-byte 16))
  (team 0 :type (unsigned-byte 16))
  (clock 0 :type (unsigned-byte 32))
  (token "" :type (counted-string 1 :external-format :utf8))
  (type 0 :type (unsigned-byte 8))
  (room "" :type (counted-string 1 :external-format :utf8))
  (players nil :type (counted-array 2 player)))

(defbinary server-backup-msg ())

(defbinary server-ping-msg ()
  (clock 0 :type (unsigned-byte 32))
  (num 0 :type (unsigned-byte 32)))

(defbinary server-ping-result-msg ()
  (ping 0 :type (unsigned-byte 16))
  (playerstotal 0 :type (unsigned-byte 32))
  (playersgame 0 :type (unsigned-byte 32)))

(defbinary server-ack-msg ())

(defbinary server-error-msg ()
  (err 0 :type (unsigned-byte 8)))

(defbinary server-command-reply-msg ())

(defbinary server-player-new-msg ())

(defbinary server-player-leave-msg ())

(defbinary server-player-update-msg ()
  (clock 0 :type (unsigned-byte 32))
  (id 0 :type (unsigned-byte 16))
  (keystate 0 :type (unsigned-byte 8))
  (upgrades 0 :type (unsigned-byte 8))
  (pos-x 0 :type (unsigned-byte 24))
  (pos-y 0 :type (unsigned-byte 24))
  (rotation 0 :type (unsigned-byte 16))
  (speed-x 0 :type (unsigned-byte 16))
  (speed-y 0 :type (unsigned-byte 16)))

(defbinary player-fire-projectile ()
  (id 0 :type (unsigned-byte 16))
  (missile-type 0 :type (unsigned-byte 8))
  (pos-x 0 :type (unsigned-byte 16))
  (pos-y 0 :type (unsigned-byte 16))
  (speed-x 0 :type (unsigned-byte 16))
  (speed-y 0 :type (unsigned-byte 16))
  (accel-x 0 :type (unsigned-byte 16))
  (accel-y 0 :type (unsigned-byte 16))
  (max-speed 0 :type (unsigned-byte 16)))

(defbinary server-player-fire-msg ()
  (clock 0 :type (unsigned-byte 32))
  (id 0 :type (unsigned-byte 16))
  (energy 0 :type (unsigned-byte 8))
  (energy-regen 0 :type (unsigned-byte 16))
  (projectiles 0 :type (counted-array 1 player-fire-projectile)))

(defbinary server-player-hit-item ()
  (id 0 :type (unsigned-byte 16))
  (health 0 :type (unsigned-byte 8))
  (health-regen 0 :type (unsigned-byte 8)))

(defbinary server-player-hit-msg ()
  (id 0 :type (unsigned-byte 8))
  (type 0 :type (unsigned-byte 8))
  (pos-x 0 :type (unsigned-byte 16))
  (pos-y 0 :type (unsigned-byte 16))
  (owner 0 :type (unsigned-byte 16))
  (players nil :type (counted-array 1 server-player-hit-item)))

(defbinary server-player-respawn-msg ())

(defbinary server-player-kill-msg ()
  (id 0 :type (unsigned-byte 16))
  (killer 0 :type (unsigned-byte 16))
  (pos-x 0 :type (unsigned-byte 16))
  (pos-y 0 :type (unsigned-byte 16)))

(defbinary server-mob-despawn-coords-msg ()
  (id 0 :type (unsigned-byte 16))
  (type 0 :type (unsigned-byte 8))
  (pos-x 0 :type (unsigned-byte 16))
  (pos-y 0 :type (unsigned-byte 16)))

(defbinary server-chat-public-msg ()
  (id 0 :type (unsigned-byte 16))
  (text "" :type (counted-string 1 :external-format :utf8)))

(defbinary server-chat-say-msg ()
  (id 0 :type (unsigned-byte 16))
  (text "" :type (counted-string 1 :external-format :utf8)))

(defbinary server-score-update-msg ()
  (id 0 :type (unsigned-byte 16))
  (score 0 :type (unsigned-byte 32))
  (earnings 0 :type (unsigned-byte 32))
  (upgrades 0 :type (unsigned-byte 16))
  (totalkills 0 :type (unsigned-byte 32))
  (totaldeaths 0 :type (unsigned-byte 32)))

(defbinary server-player-upgrade-msg ()
  (upgrades 0 :type (unsigned-byte 16))
  (type 0 :type (unsigned-byte 8))
  (speed 0 :type (unsigned-byte 8))
  (defense 0 :type (unsigned-byte 8))
  (energy 0 :type (unsigned-byte 8))
  (missile 0 :type (unsigned-byte 8)))

(defbinary server-player-type ()
  (id 0 :type (unsigned-byte 16))
  (type 0 :type (unsigned-byte 8)))

(defbinary server-player-powerup-msg ()
  (type 0 :type (unsigned-byte 8))
  (duration 0 :type (unsigned-byte 32)))

(defbinary server-player-level-msg ()
  (id 0 :type (unsigned-byte 16))
  (type 0 :type (unsigned-byte 8))
  (level 0 :type (unsigned-byte 8)))

(defbinary score-board-entry ()
  (id 0 :type (unsigned-byte 16))
  (score 0 :type (unsigned-byte 32))
  (level 0 :type (unsigned-byte 8)))

(defbinary server-score-board-msg ()
  (data 0 :type (counted-array 2 score-board-entry)))

(defbinary server-mob-update-msg ()
  (clock 0 :type (unsigned-byte 32))
  (id 0 :type (unsigned-byte 16))
  (type 0 :type (unsigned-byte 8))
  (pos-x 0 :type (unsigned-byte 16))
  (pos-y 0 :type (unsigned-byte 16))
  (speed-x 0 :type (unsigned-byte 16))
  (speed-y 0 :type (unsigned-byte 16))
  (accel-x 0 :type (unsigned-byte 16))
  (accel-y 0 :type (unsigned-byte 16))
  (max-speed 0 :type (unsigned-byte 16)))


(defbinary server-mob-update-stationary-msg ()
  (id 0 :type (unsigned-byte 16))
  (mob-type 0 :type (unsigned-byte 8))
  (pos-x 0.0 :type (single-float))
  (pos-y 0.0 :type (single-float)))

(defbinary server-mob-despawn-msg ()
  (id 0 :type (unsigned-byte 16))
  (type 0 :type (unsigned-byte 8)))


(defbinary server-event-stealth-msg ()
  (id 0 :type (unsigned-byte 16))
  (state 0 :type (unsigned-byte 8))
  (energy 0 :type (unsigned-byte 16))
  (energy-regen 0 :type (unsigned-byte 8)))

(defbinary server-event-leavehorizon-msg ()
  (type 0 :type (unsigned-byte 8))
  (id 0 :type (unsigned-byte 16)))

(defbinary server-event-repel-msg-player ()
  (id 0 :type (unsigned-byte 16))
  (keystate 0 :type (unsigned-byte 8))
  (pos-x 0 :type (unsigned-byte 16))
  (pos-y 0 :type (unsigned-byte 16))
  (rot 0 :type (unsigned-byte 16))
  (speed-x 0 :type (unsigned-byte 16))
  (speed-y 0 :type (unsigned-byte 16))
  (energy 0 :type (unsigned-byte 8))
  (energy-regen 0 :type (unsigned-byte 16))
  (player-health 0 :type (unsigned-byte 8))
  (player-health-regen 0 :type (unsigned-byte 16)))

(defbinary server-event-repel-msg-mob ()
  (id 0 :type (unsigned-byte 16))
  (type 0 :type (unsigned-byte 8))
  (pos-x 0 :type (unsigned-byte 16))
  (pos-y 0 :type (unsigned-byte 16))
  (speed-x 0 :type (unsigned-byte 16))
  (speed-y 0 :type (unsigned-byte 16))
  (accel-x 0 :type (unsigned-byte 16))
  (accel-y 0 :type (unsigned-byte 16))
  (max-speed 0 :type (unsigned-byte 16)))

(defbinary server-event-repel-msg ()
  (clock 0 :type (unsigned-byte 32))
  (id 0 :type (unsigned-byte 16))
  (pos-x 0 :type (unsigned-byte 16))
  (pos-y 0 :type (unsigned-byte 16))
  (rot 0 :type (unsigned-byte 16))
  (speed-x 0 :type (unsigned-byte 16))
  (speed-y 0 :type (unsigned-byte 16))
  (energy 0 :type (unsigned-byte 8))
  (energy-regen 0 :type (unsigned-byte 16))
  (players nil :type (counted-array 1 server-event-repel-msg-player))
  (mobs nil :type (counted-array 1 server-event-repel-msg-mob)))

(defbinary server-event-boost-msg ()
  (clock 0 :type (unsigned-byte 32))
  (id 0 :type (unsigned-byte 16))
  (boost 0 :type (unsigned-byte 8))
  (pos-x 0 :type (unsigned-byte 24))
  (pos-y 0 :type (unsigned-byte 24))
  (rot 0 :type (unsigned-byte 16))
  (speed-x 0 :type (unsigned-byte 16))
  (speed-y 0 :type (unsigned-byte 16))
  (energy 0 :type (unsigned-byte 16))
  (energy-regen 0 :type (unsigned-byte 8)))

(defbinary server-event-bounce-msg ()
  (clock 0 :type (unsigned-byte 32))
  (id 0 :type (unsigned-byte 16))
  (keystate 0 :type (unsigned-byte 16))
  (pos-x 0 :type (unsigned-byte 16))
  (pos-y 0 :type (unsigned-byte 16))
  (rotation 0 :type (unsigned-byte 16))
  (speed-x 0 :type (unsigned-byte 16))
  (speed-y 0 :type (unsigned-byte 16)))


(defbinary server-msg ()
  (command 0 :type (unsigned-byte 8))
  (body nil :type (eval (case command
                          ((0) '(server-login-msg))
                          ((1) '(server-backup-msg))
                          ((5) '(server-ping-msg))
                          ((6) '(server-ping-result-msg))
                          ((7) '(server-ack-msg))
                          ((8) '(server-error-msg))
                          ((9) '(server-command-reply-msg))
                          ((10) '(server-player-new-msg))
                          ((11) '(server-player-leave-msg))
                          ((12) '(server-player-update-msg))
                          ((13) '(server-player-fire-msg))
                          ((14) '(server-player-hit-msg))
                          ((15) '(server-player-respawn-msg))
                          ((16) '(server-placeholder-msg))
                          ((17) '(server-player-kill-msg))
                          ((18) '(server-player-upgrade-msg))
                          ((19) '(server-player-type))
                          ((20) '(server-player-powerup-msg))
                          ((21) '(server-player-level-msg))
                          ((30) '(server-placeholder-msg))
                          ((31) '(server-placeholder-msg))
                          ((32) '(server-placeholder-msg))
                          ((33) '(server-placeholder-msg))
                          ((40) '(server-event-repel-msg))
                          ((41) '(server-event-boost-msg))
                          ((42) '(server-event-bounce-msg))
                          ((43) '(server-event-stealth-msg))
                          ((44) '(server-event-leavehorizon-msg))
                          ((60) '(server-mob-update-msg))
                          ((61) '(server-mob-update-stationary-msg))
                          ((62) '(server-mob-despawn-msg))
                          ((63) '(server-mob-despawn-coords-msg))
                          ((70) '(server-chat-public-msg))
                          ((71) '(server-placeholder-msg))
                          ((72) '(server-chat-say-msg))
                          ((73) '(server-placeholder-msg))
                          ((78) '(server-placeholder-msg))
                          ((79) '(server-placeholder-msg))
                          ((80) '(server-score-update-msg))
                          ((81) '(server-score-board-msg))
                          ((82) '(server-placeholder-msg))
                          ((83) '(server-placeholder-msg))
                          ((84) '(server-placeholder-msg))
                          ((90) '(server-placeholder-msg))
                          ((91) '(server-placeholder-msg))))))


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
    (format t "Got a ping (~a), responding with pong.~%" ping-num)
    (format t "Responding with pong: ~a~%" pong)
    (send-message client pong)))

(defmethod process (client world (body server-ping-result-msg))
  (format t "Got a ping of ~a~%" (slot-value body 'ping)))  

(defmethod process (client world (body server-player-update-msg))
  (format t "Got a player update msg (~a|~a)~%"
          (slot-value body 'pos-x)
          (slot-value body 'pos-y)))

(defmethod process (client world (body server-player-hit-msg))
  (send-message client (make-player-say-command
                        :text "ouch")))


(defmethod process (client world (body server-login-msg))
  (let ((player-command (make-player-chat-command
                         :text "Hi everybody!")))
    (send-message client (make-player-ack-command)))
  (format t "Server confirmed login.~%"))

(defmethod process (client world (body server-score-update-msg))
  (format t "score updated~%"))


(defmethod process (client world (body t))
  (format t "no handler for ~a~%" body))



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


(defun main ()
  (let* ((host "wss://game-eu-s1.airma.sh/ffa2")
         (test-host "ws://localhost:3000")
         (ws-server test-host)
         (world nil)
         (client (wsd:make-client ws-server
                                  :additional-headers
                                  '(("Origin" . "https://airma.sh")))))

    (wsd:on :open client (alexandria:curry #'on-open client world))
    (wsd:on :message client (alexandria:curry #'on-message client world))
    (wsd:on :error client #'on-error)
    (wsd:on :close client #'on-close)


    (wsd:start-connection client)

    (loop
       do (let ((tick 0.25))
            (sleep tick)
            (send-message client (make-player-say-command
                                  :text "hi there"))
            (format t "bing~%")))

    (format t "Exiting.~%")))
