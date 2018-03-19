(declaim (optimize (speed 0) (debug 3) (safety 3)))
(load "~/quicklisp/setup.lisp")

;;(in-package :airmash-client)


(ql:quickload :websocket-driver-client)
(ql:quickload :alexandria)
(ql:quickload "lisp-binary")
(ql:quickload "flexi-streams")

(defpackage airmash-client
  (:use :common-lisp :lisp-binary)
  (:export :main))
  
(in-package :airmash-client)

(defun read-stream-content-into-byte-vector (stream &key ((%length length))
                                                         (initial-size 4096))
  "Return \"content\" of STREAM as freshly allocated (unsigned-byte 8) vector."
  (check-type length (or null non-negative-integer))
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

(defbinary server-chat-public-msg ()
  (id 0 :type (unsigned-byte 16))
  (text "" :type (counted-string 1)))


(defbinary server-score-update-msg ()
  (id 0 :type (unsigned-byte 16))
  (score 0 :type (unsigned-byte 32))
  (earnings 0 :type (unsigned-byte 32))
  (upgrades 0 :type (unsigned-byte 16))
  (totalkills 0 :type (unsigned-byte 32))
  (totaldeaths 0 :type (unsigned-byte 32)))

(defbinary server-player-powerup-msg ()
  (type 0 :type (unsigned-byte 8))
  (duration 0 :type (unsigned-byte 32)))

(defbinary score-board-entry ()
  (id 0 :type (unsigned-byte 16))
  (score 0 :type (unsigned-byte 32))
  (level 0 :type (unsigned-byte 8)))

(defbinary server-score-board-msg ()
  (data 0 :type (counted-array 2 score-board-entry)))

(defbinary server-mob-update-stationary-msg ()
  (id 0 :type (unsigned-byte 16))
  (mob-type 0 :type (unsigned-byte 8))
  (pos-x 0.0 :type (single-float))
  (pos-y 0.0 :type (single-float)))

(defbinary server-event-leavehorizon-msg ()
  (type 0 :type (unsigned-byte 8))
  (id 0 :type (unsigned-byte 16)))

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
                          ((17) '(server-placeholder-msg))
                          ((18) '(server-placeholder-msg))
                          ((19) '(server-placeholder-msg))
                          ((20) '(server-player-powerup-msg))
                          ((21) '(server-placeholder-msg))
                          ((30) '(server-placeholder-msg))
                          ((31) '(server-placeholder-msg))
                          ((32) '(server-placeholder-msg))
                          ((33) '(server-placeholder-msg))
                          ((40) '(server-placeholder-msg))
                          ((41) '(server-placeholder-msg))
                          ((42) '(server-event-bounce-msg))
                          ((43) '(server-placeholder-msg))
                          ((44) '(server-event-leavehorizon-msg))
                          ((60) '(server-placeholder-msg))
                          ((61) '(server-mob-update-stationary-msg))
                          ((62) '(server-placeholder-msg))
                          ((63) '(server-placeholder-msg))
                          ((70) '(server-placeholder-msg))
                          ((71) '(server-placeholder-msg))
                          ((72) '(server-placeholder-msg))
                          ((73) '(server-placeholder-msg))
                          ((78) '(server-placeholder-msg))
                          ((79) '(server-placeholder-msg))
                          ((80) '(server-score-update-msg))
                          ((81) '(server-score-board-msg))
                          ((82) '(server-placeholder-msg))
                          ((83) '(server-placeholder-msg))
                          ((84) '(server-placeholder-msg))
                          ((90) '(server-placeholder-msg))
                          ((91) '(server-chat-public-msg))))))


(defun on-message (client raw-msg)

  (format t "on-message ~a~%" raw-msg)
  
  (let ((msg nil))

    (flexi-streams:with-input-from-sequence (my-new-stream raw-msg)
      (with-wrapped-in-bit-stream (in my-new-stream :byte-order :big-endian)
        (setf msg (read-binary 'server-msg in))))


    (let ((body (slot-value msg 'body)))

      (when (typep body 'server-ping-msg)

        (let ((res (flexi-streams:with-output-to-sequence (out-raw)
                     (with-wrapped-in-bit-stream (out out-raw :byte-order :big-endian)
                       (write-binary (make-player-pong-command) out)))))


          (format t "Got a ping, responding with a pong.~%")))

          ;;(wsd:send-binary client res)))

      (when (typep body 'server-ping-result-msg)

        (format t "Got a ping of ~a~%" (slot-value body 'ping)))

      (when (typep body 'server-login-msg)

        (let ((res (flexi-streams:with-output-to-sequence (out-raw)
                     (with-wrapped-in-bit-stream (out out-raw :byte-order :big-endian)
                       (write-binary (make-server-chat-public-msg
                                      :text "Hi folks!!")
                                     out)))))

          (wsd:send-binary client res))))))

  

(defun on-error (err)
  (format t "error: ~a~%" err))

(defun on-open (client)

  (let* ((player-command (make-player-login-command
                          :name "johndoe22"
                          :horizon-x 400
                          :horizon-y 300
                          :flag "GB"))

         (res (flexi-streams:with-output-to-sequence (out-raw)
                (with-wrapped-in-bit-stream (out out-raw :byte-order :big-endian)
                  (write-binary player-command out)))))

    (wsd:send-binary client res)))


(defun on-close (&key code reason)
  (format t "Closed because '~A' (Code=~A)~%" reason code))


(defun main ()
  (let* ((ws-server "wss://game-eu-s1.airma.sh/ffa1")
         (client (wsd:make-client ws-server
                                  :additional-headers
                                  '(("Origin" . "https://airma.sh")))))

    (format t "Running...~%")

    (wsd:on :message client (alexandria:curry #'on-message client))
    (wsd:on :error client #'on-error)
    (wsd:on :open client (alexandria:curry #'on-open client))
    (wsd:on :close client #'on-close)

    (wsd:start-connection client)

    (read)

    (format t "Exiting.~%")))
