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


(defmethod fistmage:button-pressed ((this start-state) (button (eql :space)))
  (fistmage:transition-to 'start-state))


(defmethod fistmage:act ((this start-state))
  (with-slots (universe) this
    (loop repeat 10
          do (ge.phy:observe-universe (universe) 0.0014))))


(defmethod fistmage:draw ((this start-state))
  (with-slots (slime camera level) this
    (let ((camera-pos (camera-position camera (slime-position slime))))
      (gamekit:translate-canvas (gamekit:x camera-pos) (gamekit:y camera-pos)))
    (gamekit:draw-text "WTF" (gamekit:vec2 100 100))
    (render slime)
    (render level)))
