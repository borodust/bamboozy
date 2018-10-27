(cl:in-package :bamboozy)


(defgeneric make-visual-from-feature (model-feature &key &allow-other-keys))


(defclass visual ()
  ((fill-paint :initarg :fill-paint :reader fill-paint-of)
   (stroke-paint :initarg :stroke-paint :reader stroke-paint-of)
   (stroke-width :initarg :stroke-width :reader stroke-width-of)))


;;;
;;; LINE
;;;

(defclass line-visual (visual)
  ((origin :initarg :origin :reader origin-of)
   (end :initarg :end :reader end-of)))


(defmethod render ((this line-visual))
  (gamekit:draw-line (scale (origin-of this))
                     (scale (end-of this))
                     (or (stroke-paint-of this)
                         (fill-paint-of this))
                     :thickness (scale (stroke-width-of this))))


(defmethod make-visual-from-feature ((feature line-feature) &key)
  (make-instance 'line-visual
                 :origin (origin-of feature)
                 :end (end-of feature)
                 :stroke-paint (stroke-paint-of feature)
                 :stroke-width (stroke-width-of feature)))

;;;
;;; ELLIPSE
;;;
(defclass ellipse-visual (visual)
  ((origin :initarg :origin :reader origin-of)
   (x-radius :initarg :x-radius :reader x-radius-of)
   (y-radius :initarg :y-radius :reader y-radius-of)))


(defmethod render ((this ellipse-visual))
  (gamekit:draw-ellipse (scale (origin-of this))
                        (scale (x-radius-of this))
                        (scale (y-radius-of this))
                        :stroke-paint (stroke-paint-of this)
                        :fill-paint (fill-paint-of this)
                        :thickness (scale (stroke-width-of this))))


(defmethod make-visual-from-feature ((feature ellipse-feature) &key)
  (make-instance 'ellipse-visual
                 :origin (origin-of feature)
                 :x-radius (x-radius-of feature)
                 :y-radius (y-radius-of feature)
                 :stroke-paint (stroke-paint-of feature)
                 :stroke-width (stroke-width-of feature)
                 :fill-paint (fill-paint-of feature)))



;;;
;;; PATH
;;;
(defclass path-visual (visual)
  ((points :initarg :points :reader points-of)))


(defmethod render ((this path-visual))
  (gamekit:draw-polyline (mapcar #'scale (points-of this))
                         (stroke-paint-of this)
                         :thickness (scale (stroke-width-of this))))


(defmethod make-visual-from-feature ((feature path-feature) &key)
  (make-instance 'path-visual
                 :points (points-of feature)
                 :stroke-paint (stroke-paint-of feature)
                 :stroke-width (stroke-width-of feature)))



;;;
;;; RECTANGLE
;;;
(defclass rectangle-visual (visual)
  ((origin :initarg :origin :reader origin-of)
   (width :initarg :width :reader width-of)
   (height :initarg :height :reader height-of)))


(defmethod render ((this rectangle-visual))
  (gamekit:draw-rect (scale (origin-of this))
                     (scale (width-of this))
                     (scale (height-of this))
                     :stroke-paint (stroke-paint-of this)
                     :fill-paint (fill-paint-of this)
                     :thickness (scale (stroke-width-of this))))


(defmethod make-visual-from-feature ((feature rect-feature) &key)
  (make-instance 'rectangle-visual
                 :origin (origin-of feature)
                 :width (width-of feature)
                 :height (height-of feature)
                 :stroke-paint (stroke-paint-of feature)
                 :stroke-width (stroke-width-of feature)
                 :fill-paint (fill-paint-of feature)))
