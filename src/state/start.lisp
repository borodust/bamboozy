(cl:in-package :bamboozy)

(defparameter *unit-scale* 10)

(defclass ball ()
  (radius
   (body :reader %body-of)
   shape
   (color :reader %color-of)))


(defun ball-position (ball)
  (ge.phy:body-position (%body-of ball)))


(defun ball-body (ball)
  (%body-of ball))


(defmethod initialize-instance :after ((this ball) &key universe
                                                     position
                                                     (radius 1)
                                                     (mass 1)
                                                     (color (gamekit:vec4 0 0 0 1)))
  (with-slots (body shape (this-radius radius) (this-color color)) this
    (setf this-color color
          this-radius radius
          body (ge.phy:make-rigid-body universe)
          shape (ge.phy:make-circle-shape universe radius :body body :substance this)
          (ge.phy:body-position body) position)
    (ge.phy:infuse-circle-mass body mass radius)))


(defun spawn-ball (position &key (mass 1))
  (make-instance 'ball :universe (universe)
                       :position position
                       :radius 0.2
                       :mass mass
                       :color (gamekit:vec4 0.75 0.25 0.25 1)))


(defmethod render ((this ball))
  (with-slots (radius body color) this
    (let ((position (ge.phy:body-position body)))
      (gamekit:with-pushed-canvas ()
        (gamekit:translate-canvas (* (gamekit:x position) *unit-scale*) (* (gamekit:y position) *unit-scale*))
        (gamekit:draw-circle (gamekit:vec2 0 0) (* radius *unit-scale*) :fill-paint color)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass line ()
  (origin end
   (body :reader %body-of)
   shape
   (color :reader %color-of)))


(defun line-position (line)
  (ge.phy:body-position (%body-of line)))


(defun line-body (line)
  (%body-of line))


(defmethod initialize-instance :after ((this line) &key universe
                                                     origin
                                                     end
                                                     kinematic-p
                                                     (mass 1)
                                                     (color (gamekit:vec4 0 0 0 1)))
  (with-slots (body shape (this-origin origin) (this-end end)  (this-color color)) this
    (setf this-color color
          this-origin origin
          this-end end
          body (if kinematic-p (ge.phy:make-kinematic-body universe) (ge.phy:make-rigid-body universe))
          shape (ge.phy:make-segment-shape universe origin end :body body :substance this))
    (unless kinematic-p
      (ge.phy:infuse-box-mass body mass (ge.ng:vector-length (ge.ng:subt end origin)) 0.1))))


(defun spawn-line (origin end &key (mass 1) kinematic-p)
  (make-instance 'line :universe (universe)
                       :origin origin
                       :end end
                       :mass mass
                       :kinematic-p kinematic-p
                       :color (gamekit:vec4 0.75 0.25 0.25 1)))


(defmethod render ((this line))
  (with-slots (origin end color body) this
    (let ((position (ge.phy:body-position body)))
      (gamekit:with-pushed-canvas ()
        (gamekit:translate-canvas (* (gamekit:x position) *unit-scale*) (* (gamekit:y position) *unit-scale*))
        (gamekit:draw-line (ge.ng:mult origin *unit-scale*) (ge.ng:mult end *unit-scale*) color :thickness 3)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass start-state ()
  ((balls :initform nil)
   (ground :initform nil)
   (constraints :initform nil)))


(defmethod fistmage:initialize-state ((this start-state) &key)
  (with-slots (ground) this
    (setf ground (spawn-line (gamekit:vec2 1 5) (gamekit:vec2 100 1) :kinematic-p t))))


(defmethod fistmage:button-pressed ((this start-state) (button (eql :enter)))
  (with-slots (balls constraints) this
    (let ((master-ball (spawn-ball (gamekit:vec2 20 30) :mass 3)))
      (push master-ball balls)
      (loop with count = 20
            with radius = 10
            with prev-ball
            with first-ball
            for n below count
            for angle = (* (/ (* 2 pi) count) n)
            for x = (+ 20 (* (cos angle) radius))
            for y = (+ 30 (* (sin angle) radius))
            for ball = (spawn-ball (gamekit:vec2 x y) :mass 2)
            do (push ball balls)
               (unless first-ball
                 (setf first-ball ball))
               (when prev-ball
                 (push (ge.phy:make-damped-string-constraint (universe)
                                                             (ball-body prev-ball)
                                                             (ball-body ball)
                                                             2 1000 100)
                       constraints))
               (push (ge.phy:make-damped-string-constraint (universe)
                                                           (ball-body master-ball)
                                                           (ball-body ball)
                                                           10 30 10)
                     constraints)
               (setf prev-ball ball)
            finally (push (ge.phy:make-damped-string-constraint (universe)
                                                                (ball-body prev-ball)
                                                                (ball-body first-ball)
                                                                2 1000 100)
                          constraints)))))


(defmethod fistmage:button-pressed ((this start-state) (button (eql :space)))
  (fistmage:transition-to 'start-state))


(defmethod fistmage:act ((this start-state))
  (with-slots (universe) this
    (loop repeat 10
          do (ge.phy:observe-universe (universe) 0.003))))


(defmethod fistmage:draw ((this start-state))
  (with-slots (balls ground) this
    (loop for ball in balls
          do (render ball))
    (render ground)))
