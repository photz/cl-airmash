;; define a package for the shader functions, :USEing :3BGL-GLSL/CL
(cl:in-package #:shaders)

(input obj-position :vec3 :location 0)
(input color :vec3 :location 1)

(output out-color :vec3 :stage :fragment)

(uniform mvp :mat4)

(interface varyings (:out (:vertex outs)
                     :in (:fragment ins))
  (color :vec3))

(defun vertex ()
  (setf (@ outs color) color
        gl-position (* mvp (vec4 obj-position 1))))

(defun fragment ()
  (setf out-color (@ ins color)))

