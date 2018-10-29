(cl:in-package :bamboozy)


(defparameter *test-level*
  (parse-model
   (alexandria:read-file-into-string
    (asdf:system-relative-pathname :bamboozy "rsc/level/level0.svg"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass start-state ()
  ((camera :initform (make-instance 'camera))
   (slime :initform nil)
   (level :initform nil)))


(defmethod fistmage:initialize-state ((this start-state) &key)
  (with-slots (slime level) this
    (setf slime (spawn-slime (gamekit:vec2 5 5))
          level (make-level *test-level*))))


(defmethod fistmage:discard-state ((this start-state))
  (with-slots (slime level) this
    (destroy-slime slime)
    (destroy-level level)))


(defmethod fistmage:button-pressed ((this start-state) (button (eql :enter)))
  (fistmage:transition-to 'start-state))


(defmethod fistmage:button-pressed ((this start-state) (button (eql :space))))


(defmethod fistmage:button-released ((this start-state) (button (eql :space)))
  (with-slots (slime) this
    (when (slime-grounded-p slime)
      (let ((direction (gamekit:normalize
                        (gamekit:subt (fistmage:cursor-position)
                                      *viewport-half*))))
        (push-slime slime (ge.ng:mult direction 5000))))))


(defmethod fistmage:button-pressed ((this start-state) (button (eql :left-control)))
  (with-slots (slime) this
    (when (slime-grounded-p slime)
      (bind-slime slime))))


(defmethod fistmage:button-released ((this start-state) (button (eql :left-control)))
  (with-slots (slime) this
    (release-slime slime)))


(defmethod fistmage:act ((this start-state))
  (with-slots (slime) this
    (reset-slime-simulation slime)
    (let ((*slime-grounded-p* nil))
      (loop repeat 10
            do (ge.phy:observe-universe (universe) 0.0014))
      (update-slime-grounded-status slime *slime-grounded-p*))))


(defmethod fistmage:draw ((this start-state))
  (with-slots (slime camera level) this
    (let ((camera-pos (camera-position camera (slime-position slime))))
      (gamekit:translate-canvas (gamekit:x camera-pos) (gamekit:y camera-pos)))
    (render level)
    (render slime)))
