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

(defclass level-body ()
  ((shape :initform nil)))


(defmethod initialize-instance :after ((this level-body) &key shape-constructor)
  (with-slots (shape) this
    (setf shape (funcall shape-constructor this))))


(defun destroy-body (body)
  (with-slots (shape) body
    (ge.ng:dispose shape)))


(defclass line-body (level-body) ())

(defmethod make-body-from-feature ((feature line-feature) &key &allow-other-keys)
  (flet ((%shape-constructor (body)
           (ge.phy:make-segment-shape (universe)
                                      (origin-of feature)
                                      (end-of feature)
                                      :substance body)))
    (make-instance 'line-body
                   :shape-constructor #'%shape-constructor)))


(defclass path-body (level-body) ())

(defmethod make-body-from-feature ((feature path-feature) &key &allow-other-keys)
  (flet ((%shape-constructor (body)
           (ge.phy:make-polyline-shape (universe) (points-of feature)
                                       :substance body)))
    (make-instance 'path-body :shape-constructor #'%shape-constructor)))


(defclass ellipse-body (level-body) ())

(defmethod make-body-from-feature ((feature ellipse-feature) &key &allow-other-keys)
  (error "Unimplemented"))


(defclass circle-body (level-body) ())


(defmethod make-body-from-feature ((feature circle-feature) &key &allow-other-keys)
  (flet ((%shape-constructor (body)
           (ge.phy:make-circle-shape (universe) (radius-of feature)
                                     :substance body)))
    (make-instance 'circle-body :shape-constructor #'%shape-constructor)))


(defclass rectangle-body (level-body) ())


(defmethod make-body-from-feature ((feature rect-feature) &key &allow-other-keys)
  (flet ((%shape-constructor (body)
           (ge.phy:make-box-shape (universe)
                                  (width-of feature)
                                  (height-of feature)
                                  :offset (origin-of feature)
                                  :substance body)))
    (make-instance 'rectangle-body :shape-constructor #'%shape-constructor )))


;;;
;;; LEVEL
;;;
(defclass level ()
  ((visuals :initform nil :initarg :visuals)
   (bodies :initform nil :initarg :bodies)
   (spawn-point :initarg :spawn :accessor spawn-point-of)))


(defmethod render ((this level))
  (with-slots (visuals) this
    (loop for visual in visuals
          do (render visual))))


(defun make-level (level-model)
  (let (visuals bodies spawn)
    (loop for feature in (model-features level-model)
          for type = (feature-type-of feature)
          when (equalp (id-of feature) "slime-spawn")
            do (setf spawn (origin-of feature))
          when (or (eq :obstacle type) (eq :background type))
            do (push (make-visual-from-feature feature) visuals)
          when (or (eq :obstacle type) (eq :controller type))
            do (push (make-body-from-feature feature) bodies))
    (make-instance 'level :visuals visuals
                          :bodies bodies
                          :spawn (or spawn (gamekit:vec2 5 5)))))


(defun destroy-level (level)
  (with-slots (bodies) level
    (loop for body in bodies
          do (destroy-body body))))
