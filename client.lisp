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
