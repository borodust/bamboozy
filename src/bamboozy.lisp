(cl:in-package :bamboozy)


(defvar *game* nil)


(defclass game ()
  ((universe)))


(defun universe ()
  (slot-value *game* 'universe))


(defmethod initialize-instance :after ((this game) &key)
  (with-slots (universe) this
    (flet ((%on-pre-solve (this-shape that-shape)
             (declare (ignore this-shape that-shape))
             (setf (ge.phy:collision-friction) 1000)
             t))
      (setf universe (ge.phy:make-universe :2d :on-pre-solve #'%on-pre-solve)
            (ge.phy:gravity universe) (gamekit:vec2 0 -9.81)))))


(fistmage:defgame (bamboozy start-state)
  (:viewport-title "BAMBOOZY")
  (:viewport-width 1024)
  (:viewport-height 768))


(defmethod initialize-instance ((this bamboozy) &rest args
                                &key &allow-other-keys)
  (destructuring-bind (&rest args &key depends-on &allow-other-keys) args
    (apply #'call-next-method this :depends-on (append (list 'ge.phy:physics-system) depends-on)
           args)))


(defmethod fistmage:initialize-game ((this bamboozy))
  (setf *game* (make-instance 'game)))


(defun play ()
  (fistmage:start 'bamboozy :resizable t))
