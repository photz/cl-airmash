(defpackage ecs
  (:use :cl)
  (:export #:world
           #:new-entity
           #:set-component
           #:get-component
           #:update-components
           #:add-entity
           #:remove-entity))

(defpackage airmash-client
  (:use :cl :lisp-binary)
  (:export :main))
