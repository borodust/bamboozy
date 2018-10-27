(cl:in-package :bamboozy)


(defparameter *unit-scale* 10)


(defgeneric scale (value))


(defmethod scale ((null null))
  null)


(defmethod scale ((number number))
  (* *unit-scale* number))


(defmethod scale ((vec2 gamekit:vec2))
  (gamekit:vec2 (scale (gamekit:x vec2))
                (scale (gamekit:y vec2))))


(defgeneric render (object))
