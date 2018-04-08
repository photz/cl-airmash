(asdf:defsystem #:cl-airmash
  :description "Alternative client for the Airmash top-down shooter"
  :serial t
  :components ((:file "package")
               (:file "math-helper")
               (:file "opengl-helper")
               (:file "shaders")
               (:file "server-messages")
               (:file "player-messages")
               (:file "client")
               (:file "handlers")
               (:file "ecs")
               (:file "main"))
  :depends-on (#:lisp-binary
               #:alexandria
               #:flexi-streams
               #:cl-opengl
               #:3bgl-shader
               #:glop
               #:websocket-driver-client))
