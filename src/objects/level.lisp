(cl:in-package :bamboozy)


;;;
;;; LEVEL
;;;
(defclass level ()
  ((visuals :initform nil :initarg :visuals)
   (spawn-point :initform (gamekit:vec2 5 5) :accessor player-spawn-point-of)
   (enemy-spawns :initform nil :accessor enemy-spawn-points-of)))


(defmethod render ((this level))
  (with-slots (visuals) this
    (loop for visual in visuals
          do (render visual))))


(defgeneric infuse-level-feature (&key &allow-other-keys))


(defun make-level (level-model)
  (make-instance 'level :visuals (loop for feature in (model-features level-model)
                                       for type = (feature-type-of feature)
                                       when (or (eq :obstacle type) (eq :background type))
                                         collect (make-visual-from-feature feature))))
