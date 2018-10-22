(cl:in-package :bamboozy)


(defclass start-state () ())


(defmethod fistmage:draw ((this start-state))
  (gamekit:draw-text "BAMBOOZY" (gamekit:vec2 400 400)))
