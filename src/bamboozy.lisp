(cl:in-package :bamboozy)


(fistmage:defgame (bamboozy start-state)
  (:viewport-title "BAMBOOZY")
  (:viewport-width *viewport-width*)
  (:viewport-height *viewport-height*))


(defmethod initialize-instance ((this bamboozy) &rest args
                                &key &allow-other-keys)
  (destructuring-bind (&rest args &key depends-on &allow-other-keys) args
    (apply #'call-next-method this :depends-on (append (list 'ge.phy:physics-system) depends-on)
           args)))


(defmethod fistmage:initialize-game ((this bamboozy))
  (setf *game* (make-instance 'game)))


(defmethod fistmage:destroy-game ((this bamboozy))
  (destroy-game *game*)
  (setf *game* nil))


(defun play ()
  (fistmage:start 'bamboozy :resizable t))
