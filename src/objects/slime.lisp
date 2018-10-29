(cl:in-package :bamboozy)

(defparameter *slime-skin-subdivision* 20)

(defparameter *slime-core-stiffness* 1500)

(defparameter *slime-core-damping* 20)

(defparameter *slime-grounded-p* nil)

(defparameter *slime-model*
  (parse-model
   (alexandria:read-file-into-string
    (asdf:system-relative-pathname :bamboozy "rsc/model/bamboozy.svg"))))

;;;
;;; CORE
;;;
(defclass slime-core ()
  ((body :initarg :body :reader slime-core-body)
   (shape :initarg :shape)
   (slime :initarg :slime)))


(defun make-slime-core (position slime radius)
  (let* ((body (ge.phy:make-rigid-body (universe)))
         (shape (ge.phy:make-circle-shape (universe) radius
                                          :body body)))
    (setf (ge.phy:body-position body) position)
    (make-instance 'slime-core :body body
                               :shape shape
                               :slime slime)))


(defun slime-core-position (core)
  (with-slots (body shape) core
    (ge.phy:body-position body)))


(defun destroy-slime-core (core)
  (with-slots (body shape) core
    (ge.ng:dispose shape)
    (ge.ng:dispose body)))


(defmethod collide ((this slime-core) (that level-body))
  (with-slots (slime) this
    (register-colliding-slime-shape slime *this-shape* *that-shape*)
    (setf *slime-grounded-p* t)))


(defmethod collide ((that level-body) (this slime-core))
  (collide this that))

;;;
;;; SKIN
;;;
(defclass slime-body ()
  ((shapes :initarg :shapes :reader slime-body-shapes)
   (constraints :initarg :constraints)
   (fill :initarg :fill)
   (stroke :initarg :stroke)
   (thickness :initarg :thickness)))


(defun make-slime-body (core origin radius fill stroke thickness)
  (let (shapes
        constraints
        (core-offset (gamekit:subt origin (slime-core-position core))))
    (labels ((%make-shape (point)
               (let* ((body (ge.phy:make-rigid-body (universe)))
                      (shape (ge.phy:make-circle-shape (universe) (/ thickness 2)
                                                       :body body
                                                       :substance core)))
                 (setf (ge.phy:body-position body) (gamekit:add origin point core-offset))
                 (push (ge.phy:make-damped-string-constraint (universe)
                                                             (slime-core-body core)
                                                             body
                                                             radius
                                                             *slime-core-stiffness*
                                                             *slime-core-damping*)
                       constraints)
                 (push (ge.phy:make-slide-constraint (universe)
                                                     (slime-core-body core)
                                                     body
                                                     (/ radius 10)
                                                     radius)
                       constraints)
                 shape))
             (%link-segment (prev-shape next-shape)
               (let ((rest-length (ge.ng:vector-length (gamekit:subt (shape-position next-shape)
                                                                     (shape-position prev-shape)))))
                 (push (ge.phy:make-slide-constraint (universe)
                                                     (ge.phy:shape-body prev-shape)
                                                     (ge.phy:shape-body next-shape)
                                                     (/ rest-length 10)
                                                     rest-length)
                       constraints)))
             (%next-point (num)
               (let ((angle (* (/ (* 2 pi) *slime-skin-subdivision*) num)))
                 (gamekit:vec2 (* (cos angle) radius)
                               (* (sin angle) radius)))))
      (loop with prev-point = (%next-point 0)
            with first-shape = (%make-shape prev-point)
            with prev-shape = first-shape
            for idx from 1 below *slime-skin-subdivision*
            for next-point = (%next-point idx)
            for next-shape = (%make-shape next-point)
            do (%link-segment prev-shape next-shape)
               (push prev-shape shapes)
               (setf prev-point next-point
                     prev-shape next-shape)
            finally (%link-segment prev-shape first-shape)
                    (push prev-shape shapes)))
    (make-instance 'slime-body :shapes shapes
                               :constraints constraints
                               :fill fill
                               :stroke stroke
                               :thickness thickness)))


(defun destroy-slime-body (skin)
  (with-slots (shapes constraints) skin
    (loop for shape in shapes
          do (destroy-shape shape))
    (loop for constraint in constraints
          do (ge.ng:dispose constraint))))


(defun calc-control-point (prev origin end &optional (scale 1.0))
  (let* ((base-angle (acos (ge.ng:dot (ge.ng:normalize (ge.ng:subt prev origin))
                                      (ge.ng:normalize (ge.ng:subt end origin)))))
         (tangent-angle (/ (- pi base-angle) 2))
         (vec (ge.ng:mult (ge.ng:subt end origin) scale)))
    (ge.ng:add origin (ge.ng:mult (ge.ng:euler-angle->mat2 (- tangent-angle)) vec))))


(defmethod render ((this slime-body))
  (with-slots (shapes fill stroke thickness) this
    (let ((first-shape (first shapes)))
      (ge.vg:path
        (ge.vg:move-to (scale (shape-position first-shape)))
        (loop with prev-shape = first-shape
              for next-shape in (rest shapes)
              for origin = (scale (shape-position prev-shape))
              for end = (scale (shape-position next-shape))
              do (ge.vg:bezier-to origin end end)
                 (setf prev-shape next-shape)
              finally (ge.vg:bezier-to (scale (shape-position prev-shape))
                                       (scale (shape-position first-shape))
                                       (scale (shape-position first-shape))))
        (setf (ge.vg:stroke-paint) stroke
              (ge.vg:fill-paint) fill
              (ge.vg:stroke-width) (scale thickness))
        (ge.vg:stroke-path)
        (ge.vg:fill-path)))))


;;;
;;; SLIME
;;;
(defclass slime ()
  ((core :initform nil)
   (skin :initform nil)
   (grounded-p :initform nil)
   (colliding-shapes :initform nil)
   (bounding-constraints :initform nil)))


(defun slime-position (slime)
  (with-slots (core) slime
    (slime-core-position core)))


(defmethod initialize-instance :after ((this slime) &key position)
  (with-slots (core skin) this
    (let ((slime-core (find-model-feature-by-id *slime-model* "slime-core"))
          (slime-body (find-model-feature-by-id *slime-model* "slime-body")))
      (setf core (make-slime-core (gamekit:add (origin-of slime-core)
                                               position)
                                  this
                                  (radius-of slime-core))
            skin (make-slime-body core (gamekit:add (origin-of slime-body)
                                                    position)
                                  (radius-of slime-body)
                                  (fill-paint-of slime-body)
                                  (stroke-paint-of slime-body)
                                  (stroke-width-of slime-body))))))


(defun update-slime-grounded-status (slime grounded-p)
  (with-slots ((this-grounded-p grounded-p)) slime
    (setf this-grounded-p grounded-p)))


(defun slime-grounded-p (slime)
  (with-slots (grounded-p) slime
    grounded-p))


(defun push-slime (slime force)
  (with-slots (core skin) slime
    (ge.phy:apply-force (slime-core-body core) force)
    (loop for body-shape in (slime-body-shapes skin)
          do (ge.phy:apply-force (ge.phy:shape-body body-shape) force))))


(defun bind-slime (slime)
  (with-slots (colliding-shapes bounding-constraints) slime
    (loop for (this . that) in colliding-shapes
          do (push (ge.phy:make-pin-constraint (universe)
                                               (ge.phy:shape-body this)
                                               (ge.phy:shape-body that)
                                               :that-anchor (shape-position this))
                   bounding-constraints))))


(defun release-slime (slime)
  (with-slots (bounding-constraints) slime
    (loop for constraint in bounding-constraints
          do (ge.ng:dispose constraint))
    (setf bounding-constraints nil)))


(defun register-colliding-slime-shape (slime this-shape that-shape)
  (with-slots (colliding-shapes) slime
    (push (cons this-shape that-shape) colliding-shapes)))


(defun reset-slime-simulation (slime)
  (with-slots (colliding-shapes) slime
    (setf colliding-shapes nil)))


(defmethod render ((this slime))
  (with-slots (skin) this
    (gamekit:with-pushed-canvas ()
      (render skin))))


(defun spawn-slime (position)
  (make-instance 'slime :position position))


(defun destroy-slime (slime)
  (with-slots (skin core) slime
    (destroy-slime-core core)
    (destroy-slime-body skin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
