(in-package :airmash-client)


(defclass airmash-client ()
  ((player-name :initarg :player-name)
   (player-flag :initarg :player-flag)
   (ws :initform nil)
   (player-id :initform nil)
   (keyseq :initform 0)
   (on-message :initarg :on-message)))

    

(defmethod initialize-instance :after ((c airmash-client) &key host)
  (let* ((headers '(("Origin" . "https://airma.sh")))
         (ws (wsd:make-client host :additional-headers headers)))
    
    (setf (slot-value c 'ws) ws)

    (wsd:on :open ws (alexandria:curry #'on-open c))
    (wsd:on :message ws (alexandria:curry #'on-message c))
    (wsd:on :error ws (alexandria:curry #'on-message c))
    (wsd:on :close ws (alexandria:curry #'on-message c))

    (wsd:start-connection ws)))


(defmethod send-message ((c airmash-client) player-command)
  (let ((res (flexi-streams:with-output-to-sequence (out-raw)
               (with-wrapped-in-bit-stream (out out-raw :byte-order :big-eendian)
                 (write-binary player-command out)))))

    (wsd:send-binary (slot-value c 'ws) res)))



(defmethod on-message ((c airmash-client) raw-msg)
  (let ((msg nil))
    (flexi-streams:with-input-from-sequence (my-new-stream raw-msg)
      (with-wrapped-in-bit-stream (in my-new-stream :byte-order :big-endian)
        (setf msg (read-binary 'server-msg in))))

    (let ((body (slot-value msg 'body)))

      (when (typep body 'server-login-msg)
        (setf (slot-value c 'player-id) (slot-value body 'id)))

      (funcall (slot-value c 'on-message) c body))))



(defmethod on-error ((c airmash-client) err)
  (format t "error: ~a~%" err))



(defmethod on-open ((c airmash-client))
  (format t "Connected~%")
  (send-message c (make-player-login-command
                   :name (slot-value c 'player-name)
                   :horizon-x (/ 1920 2)
                   :horizon-y (/ 1920 2)
                   :flag (slot-value c 'player-flag))))


(defmethod on-close ((c airmash-client) &key code reason)
  (format t "Closed because '~A' (Code=~A)~%" reason code))


(defmethod send-key ((c airmash-client) key pressed)
  (check-type key keyword)
  (check-type pressed boolean)

  (let* ((val (case key
                (:up 1)
                (:down 2)
                (:left 3)
                (:right 4)
                (:space 5)))


         (cmd (make-player-key-command :seq (slot-value c 'keyseq)
                                       :key val
                                       :state (if pressed 1 0))))

    (send-message c cmd)

    (incf (slot-value c 'keyseq))))

        

        
        
          
