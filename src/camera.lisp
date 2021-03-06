(cl:in-package :bamboozy)


(defparameter *camera-speed* 8)


(defclass camera ()
  ((current-position :initform (gamekit:vec2 0 0))
   (timestamp :initform 0d0)))


(defun camera-position (camera player-pos)
  (with-slots (current-position timestamp) camera
    (let* ((cursor-offset (gamekit:div (gamekit:subt *viewport-half* (fistmage:cursor-position)) 2))
           (current-seconds (bodge-util:real-time-seconds))
           (time-delta (- current-seconds timestamp))
           (target-pos (gamekit:add cursor-offset (gamekit:mult (scale player-pos) -1) *viewport-half*))
           (target-vec (gamekit:subt target-pos current-position))
           (target-distance (ge.ng:vector-length target-vec)))
      (when (> target-distance 0d0)
        (let ((target-offset (gamekit:mult target-vec
                                           (/ (min (* time-delta *camera-speed* target-distance)
                                                   target-distance)
                                              target-distance))))
          (incf (gamekit:x current-position) (gamekit:x target-offset))
          (incf (gamekit:y current-position) (gamekit:y target-offset))
          (setf timestamp current-seconds)))
      current-position)))
