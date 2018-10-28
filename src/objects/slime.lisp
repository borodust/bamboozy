(cl:in-package :bamboozy)

(defparameter *slime-skin-subdivision* 14)

(defparameter *slime-model*
  (parse-model
   (alexandria:read-file-into-string
    (asdf:system-relative-pathname :bamboozy "rsc/model/bamboozy.svg"))))

;;;
;;; CORE
;;;
(defclass slime-core ()
  ((body :initarg :body :reader slime-core-body)))


(defun make-slime-core (position)
  (let ((body (ge.phy:make-rigid-body (universe))))
    (setf (ge.phy:body-position body) position)
    (make-instance 'slime-core :body body)))


(defun slime-core-position (core)
  (with-slots (body) core
    (ge.phy:body-position body)))


(defun destroy-slime-core (core)
  (with-slots (body) core
    (ge.ng:dispose body)))

;;;
;;; SKIN
;;;
(defclass slime-skin-segment ()
  ((origin-shape :initarg :origin-shape)
   (end-shape :initarg :end-shape)
   (thickness :initarg :thickness)))


(defun make-slime-skin-segment (origin-shape end-shape thickness)
  (make-instance 'slime-skin-segment :origin-shape origin-shape
                                     :end-shape end-shape
                                     :thickness thickness))


(defmethod render ((this slime-skin-segment))
  (with-slots (origin-shape end-shape thickness) this
    (let* ((origin (shape-position origin-shape))
           (end (shape-position end-shape)))
      (gamekit:draw-line (scale origin) (scale end) (gamekit:vec4 0 0 0 1)
                         :thickness (scale thickness)))))


(defclass slime-skin ()
  ((shapes :initarg :shapes)
   (segments :initarg :segments)
   (constraints :initarg :constraints)))


(defun make-slime-skin (core origin radius thickness)
  (let (segments
        shapes
        constraints
        (core-offset (gamekit:subt origin (slime-core-position core))))
    (labels ((%make-shape (point)
               (let* ((body (ge.phy:make-rigid-body (universe)))
                      (shape (ge.phy:make-circle-shape (universe) (/ thickness 2)
                                                       :body body)))
                 (setf (ge.phy:body-position body) (gamekit:add point core-offset))
                 (push (ge.phy:make-damped-string-constraint (universe)
                                                             (slime-core-body core)
                                                             body
                                                             radius 1000 10)
                       constraints)
                 shape))
             (%make-segment (prev-shape next-shape)
               (let ((rest-length (ge.ng:vector-length (gamekit:subt (shape-position next-shape)
                                                                     (shape-position prev-shape)))))
                 (push (ge.phy:make-slide-constraint (universe)
                                                     (ge.phy:shape-body prev-shape)
                                                     (ge.phy:shape-body next-shape)
                                                     0 rest-length)
                       constraints))
               (push (make-slime-skin-segment prev-shape next-shape thickness) segments)
               (push prev-shape shapes))
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
            do (%make-segment prev-shape next-shape)
               (setf prev-point next-point
                     prev-shape next-shape)
            finally (%make-segment prev-shape first-shape)))
    (make-instance 'slime-skin :shapes shapes
                               :segments segments
                               :constraints constraints)))


(defun destroy-slime-skin (skin)
  (with-slots (shapes constraints) skin
    (loop for shape in shapes
          do (destroy-shape shape))
    (loop for constraint in constraints
          do (ge.ng:dispose constraint))))


(defmethod render ((this slime-skin))
  (with-slots (segments) this
    (loop for segment in segments
          do (render segment))))


;;;
;;; SLIME
;;;
(defclass slime ()
  ((core :initform nil)
   (skin :initform nil)))


(defun slime-position (slime)
  (with-slots (core) slime
    (slime-core-position core)))


(defmethod initialize-instance :after ((this slime) &key)
  (with-slots (core skin) this
    (let ((slime-core (find-model-feature-by-id *slime-model* "slime-core"))
          (slime-body (find-model-feature-by-id *slime-model* "slime-body")))
      (setf core (make-slime-core (origin-of slime-core))
            skin (make-slime-skin core (origin-of slime-body)
                                  (radius-of slime-body)
                                  (stroke-width-of slime-body))))))


(defmethod render ((this slime))
  (with-slots (skin) this
    (gamekit:with-pushed-canvas ()
      (render skin))))


(defun spawn-slime (position)
  (declare (ignore position))
  (make-instance 'slime))


(defun destroy-slime (slime)
  (with-slots (skin core) slime
    (destroy-slime-core core)
    (destroy-slime-skin skin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
