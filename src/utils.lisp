(cl:in-package :bamboozy)

(declaim (special *this-shape*
                  *that-shape*))


(defparameter *unit-scale* 100)

(defgeneric scale (value))
(defgeneric unscale (value))


(defmethod scale ((null null))
  null)


(defmethod unscale ((null null))
  null)


(defmethod scale ((number number))
  (* *unit-scale* number))


(defmethod unscale ((number number))
  (/ number *unit-scale*))


(defmethod scale ((vec2 gamekit:vec2))
  (gamekit:vec2 (scale (gamekit:x vec2))
                (scale (gamekit:y vec2))))


(defmethod unscale ((vec2 gamekit:vec2))
  (gamekit:vec2 (unscale (gamekit:x vec2))
                (unscale (gamekit:y vec2))))


(defgeneric render (object))


(defgeneric pre-collide (this-object that-object)
  (:method (this that)
    (declare (ignore this that))
    (setf (ge.phy:collision-friction) 1.0)
    t))


(defgeneric collide (this-object that-object)
  (:method (this that)
    (declare (ignore this that))))


(defun shape-position (shape)
  (ge.phy:body-position (ge.phy:shape-body shape)))


(defun shape-angle (shape)
  (let ((rotation (ge.phy:body-rotation (ge.phy:shape-body shape))))
    (atan (gamekit:y rotation) (gamekit:x rotation))))


(defun destroy-shape (shape)
  (let ((body (ge.phy:shape-body shape)))
    (ge.ng:dispose shape)
    (ge.ng:dispose body)))
