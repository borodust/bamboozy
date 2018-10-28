(cl:in-package :bamboozy)


(defgeneric make-visual-from-feature (model-feature &key &allow-other-keys))


(defclass visual ()
  ((fill-paint :initform nil :initarg :fill-paint :reader fill-paint-of)
   (stroke-paint :initform nil :initarg :stroke-paint :reader stroke-paint-of)
   (stroke-width :initform nil :initarg :stroke-width :reader stroke-width-of)))


;;;
;;; LINE
;;;

(defclass line-visual (visual)
  ((origin :initarg :origin :reader origin-of)
   (end :initarg :end :reader end-of)))


(defun make-line-visual (origin end &rest opts &key &allow-other-keys)
  (apply #'make-instance 'line-visual :origin origin :end end opts))


(defmethod render ((this line-visual))
  (gamekit:draw-line (scale (origin-of this))
                     (scale (end-of this))
                     (or (stroke-paint-of this)
                         (fill-paint-of this))
                     :thickness (scale (stroke-width-of this))))


(defmethod make-visual-from-feature ((feature line-feature) &key)
  (make-line-visual (origin-of feature) (end-of feature)
                    :stroke-paint (stroke-paint-of feature)
                    :stroke-width (stroke-width-of feature)))

;;;
;;; ELLIPSE
;;;
(defclass ellipse-visual (visual)
  ((origin :initarg :origin :reader origin-of)
   (x-radius :initarg :x-radius :reader x-radius-of)
   (y-radius :initarg :y-radius :reader y-radius-of)))


(defun make-ellipse-visual (origin x-radius y-radius &rest opts &key &allow-other-keys)
  (apply #'make-instance 'ellipse-visual :x-radius x-radius
                                         :y-radius y-radius
                                         :origin origin
                                         opts))


(defmethod render ((this ellipse-visual))
  (gamekit:draw-ellipse (scale (origin-of this))
                        (scale (x-radius-of this))
                        (scale (y-radius-of this))
                        :stroke-paint (stroke-paint-of this)
                        :fill-paint (fill-paint-of this)
                        :thickness (scale (stroke-width-of this))))


(defmethod make-visual-from-feature ((feature ellipse-feature) &key)
  (make-ellipse-visual (origin-of feature) (x-radius-of feature) (y-radius-of feature)
                       :stroke-paint (stroke-paint-of feature)
                       :stroke-width (stroke-width-of feature)
                       :fill-paint (fill-paint-of feature)))


;;;
;;; CIRCLE
;;;
(defclass circle-visual (visual)
  ((origin :initarg :origin :reader origin-of)
   (radius :initarg :radius :reader radius-of)))


(defun make-circle-visual (origin radius &rest opts &key &allow-other-keys)
  (apply #'make-instance 'circle-visual :radius radius
                                        :origin origin
                                        opts))

(defmethod (setf origin-of) ((value gamekit:vec2) (visual circle-visual))
  (let ((origin (origin-of visual)))
    (setf (gamekit:x origin) (gamekit:x value)
          (gamekit:y origin) (gamekit:y value))
    value))


(defmethod render ((this circle-visual))
  (gamekit:draw-circle (scale (origin-of this))
                       (scale (radius-of this))
                       :stroke-paint (stroke-paint-of this)
                       :fill-paint (fill-paint-of this)
                       :thickness (scale (stroke-width-of this))))


(defmethod make-visual-from-feature ((feature circle-feature) &key)
  (make-circle-visual (origin-of feature) (radius-of feature)
                      :stroke-paint (stroke-paint-of feature)
                      :stroke-width (stroke-width-of feature)
                      :fill-paint (fill-paint-of feature)))

;;;
;;; PATH
;;;
(defclass path-visual (visual)
  ((points :initarg :points :reader points-of)))


(defun make-path-visual (points &rest opts &key &allow-other-keys)
  (apply #'make-instance 'path-visual :points points opts))


(defmethod render ((this path-visual))
  (gamekit:draw-polyline (mapcar #'scale (points-of this))
                         (stroke-paint-of this)
                         :thickness (scale (stroke-width-of this))))


(defmethod make-visual-from-feature ((feature path-feature) &key)
  (make-path-visual (points-of feature)
                    :stroke-paint (stroke-paint-of feature)
                    :stroke-width (stroke-width-of feature)))



;;;
;;; RECTANGLE
;;;
(defclass rectangle-visual (visual)
  ((origin :initarg :origin :reader origin-of)
   (width :initarg :width :reader width-of)
   (height :initarg :height :reader height-of)))


(defun make-rectangle-visual (origin width height &rest opts &key &allow-other-keys)
  (apply #'make-instance 'rectangle-visual
         :origin origin
         :width width
         :height height
         opts))


(defmethod render ((this rectangle-visual))
  (gamekit:draw-rect (scale (origin-of this))
                     (scale (width-of this))
                     (scale (height-of this))
                     :stroke-paint (stroke-paint-of this)
                     :fill-paint (fill-paint-of this)
                     :thickness (scale (stroke-width-of this))))


(defmethod make-visual-from-feature ((feature rect-feature) &key)
  (make-rectangle-visual (origin-of feature) (width-of feature) (height-of feature)
                         :stroke-paint (stroke-paint-of feature)
                         :stroke-width (stroke-width-of feature)
                         :fill-paint (fill-paint-of feature)))
