(cl:in-package :bamboozy)


(defparameter *viewport-width* 1024)
(defparameter *viewport-height* 768)
(defparameter *viewport-half* (gamekit:vec2 (/ 1024 2) (/ 768 2)))


(defvar *game* nil)


(defclass game ()
  ((universe)))


(defun universe ()
  (slot-value *game* 'universe))


(defmethod initialize-instance :after ((this game) &key)
  (with-slots (universe) this
    (flet ((%on-pre-solve (this-shape that-shape)
             (pre-collide (ge.phy:shape-substance this-shape) (ge.phy:shape-substance that-shape)))
           (%on-post-solve (this-shape that-shape)
             (collide (ge.phy:shape-substance this-shape)
                      (ge.phy:shape-substance that-shape))))
      (setf universe (ge.phy:make-universe :2d :on-pre-solve #'%on-pre-solve
                                               :on-post-solve #'%on-post-solve)
            (ge.phy:gravity universe) (gamekit:vec2 0 -9.81)))))


(defun destroy-game (game)
  (declare (ignore game)))
