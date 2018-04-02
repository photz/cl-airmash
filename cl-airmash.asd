(asdf:defsystem :cl-airmash
  :description "Alternative client for the Airmash top-down shooter"
  :serial t
  :components ((:file "package")
               (:file "server-messages")
               (:file "player-messages")
               (:file "ecs")
               (:file "client"))
  :depends-on (:lisp-binary
               :alexandria
               :flexi-streams
               :websocket-driver-client))
