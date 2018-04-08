(defpackage #:math-helper
  (:use :cl)
  (:export #:make-mat4
           #:make-vec3
           #:make-vec4
           #:dot
           #:norm
           #:normalize
           #:cross
           #:vdiff
           #:vadd
           #:vprod
           #:mvprod
           #:mprod
           #:translation
           #:rotation
           #:perspective
           #:ortho
           #:look-at
           #:d->r
           #:vx
           #:vy
           #:vz
           #:vw))

(defpackage #:opengl-helper
  (:use :cl)
  (:export #:link-program
           #:enable-vertex-array
           #:load-buffer-array))


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
  (:use :cl :lisp-binary :math-helper :opengl-helper)
  (:export :main))
