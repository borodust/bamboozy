(cl:in-package :bamboozy)


(defparameter *slime-model*
  (parse-model
   (alexandria:read-file-into-string
    (asdf:system-relative-pathname :bamboozy "rsc/model/bamboozy.svg"))))


(defclass slime ()
  ((visuals :initform nil)))


(defmethod initialize-instance :after ((this slime) &key)
  (with-slots (visuals) this
    (setf visuals (loop for feature in (model-features *slime-model*)
                        for type = (feature-type-of feature)
                        when (or (eq :obstacle type) (eq :background type))
                          collect (make-visual-from-feature feature)))))


(defmethod render ((this slime))
  (with-slots (visuals) this
    (loop for visual in visuals
          do (render visual))))


(defun spawn-slime (position)
  (declare (ignore position))
  (make-instance 'slime))


(defun destroy-slime (slime)
  (declare (ignore slime)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
