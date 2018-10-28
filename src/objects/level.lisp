(cl:in-package :bamboozy)


;;;
;;; CONTROLLERS
;;;
(defclass controller ()
  ((id :initarg :id :initform (error ":id missing") :reader id-of)
   (shape :initarg :type :reader shape-of)))


(defmethod initialize-instance :after ((this controller) &key shape-constructor)
  (with-slots (shape) this
    (setf shape (funcall shape-constructor this))))


(defclass line-controller (controller)
  ((origin :initarg :origin)
   (end :initarg :end)))


(defun make-line-controller (id origin end)
  (flet ((%make-shape (controller)
           (ge.phy:make-segment-shape (universe) origin end
                                      :substance controller)))
    (make-instance 'line-controller
                   :origin origin
                   :end end
                   :id id
                   :shape-constructor #'%make-shape)))

;;;
;;; BODIES
;;;
(defgeneric make-body-from-feature (feature &key &allow-other-keys))

(defclass body ()
  ((shape :initarg :shape :initform (error ":shape missing"))
   (body :initarg :body :initform nil)))


(defun destroy-body (body)
  (with-slots (shape) body
    (ge.ng:dispose shape)))


(defclass line-body (body) ())

(defmethod make-body-from-feature ((feature line-feature) &key &allow-other-keys)
  (make-instance 'line-body
                 :shape (ge.phy:make-segment-shape (universe)
                                                   (origin-of feature)
                                                   (end-of feature))))


(defclass path-body (body) ())

(defmethod make-body-from-feature ((feature path-feature) &key &allow-other-keys)
  (make-instance 'path-body
                 :shape (ge.phy:make-polyline-shape (universe) (points-of feature))))


(defclass ellipse-body (body) ())

(defmethod make-body-from-feature ((feature ellipse-feature) &key &allow-other-keys)
  (error "Unimplemented"))


(defclass circle-body (body) ())


(defmethod make-body-from-feature ((feature circle-feature) &key &allow-other-keys)
  (make-instance 'circle-body
                 :shape (ge.phy:make-circle-shape (universe) (radius-of feature))))


(defclass rectangle-body (body) ())


(defmethod make-body-from-feature ((feature rect-feature) &key &allow-other-keys)
  (make-instance 'rectangle-body
                 :shape (ge.phy:make-box-shape (universe)
                                               (width-of feature)
                                               (height-of feature)
                                               :offset (origin-of feature))))


;;;
;;; LEVEL
;;;
(defclass level ()
  ((visuals :initform nil :initarg :visuals)
   (bodies :initform nil :initarg :bodies)
   (spawn-point :initform (gamekit:vec2 5 5) :accessor player-spawn-point-of)
   (enemy-spawns :initform nil :accessor enemy-spawn-points-of)))


(defmethod render ((this level))
  (with-slots (visuals) this
    (loop for visual in visuals
          do (render visual))))


(defun make-level (level-model)
  (let (visuals bodies)
    (loop for feature in (model-features level-model)
          for type = (feature-type-of feature)
          when (or (eq :obstacle type) (eq :background type))
            do (push (make-visual-from-feature feature) visuals)
          when (or (eq :obstacle type) (eq :controller type))
            do (push (make-body-from-feature feature) bodies))
    (make-instance 'level :visuals visuals :bodies bodies)))


(defun destroy-level (level)
  (with-slots (bodies) level
    (loop for body in bodies
          do (destroy-body body))))
