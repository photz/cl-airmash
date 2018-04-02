(asdf:defsystem :cl-airmash
  :description "Alternative client for the Airmash top-down shooter"
  :serial t
  :components ((:file "package")
               (:file "client"))
  :depends-on (:lisp-binary
               :alexandria
               :flexi-streams
               :qua
               :websocket-driver-client))
