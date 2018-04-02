(in-package :airmash-client)

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
