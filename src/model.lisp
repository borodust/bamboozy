(cl:in-package :bamboozy)


(declaim (special *model-height*))

(defun parse-color (string &optional (opacity 1.0))
  (unless (or (null string) (equalp string "none"))
    (let* ((color-string (subseq string 1))
           (hex-color (parse-number:parse-number color-string :radix 16)))
      (flet ((read-color (num)
               (float (/ (ldb (byte 8 (* num 8)) hex-color) 255) 0f0)))
        (alexandria:switch ((length color-string))
          (8 (gamekit:vec4 (read-color 3) (read-color 2) (read-color 1) (read-color 0)))
          (6 (gamekit:vec4 (read-color 2) (read-color 1) (read-color 0) opacity))
          (2 (gamekit:vec4 (read-color 0) (read-color 0) (read-color 0) opacity))
          (t (error "Unrecognized color encoding")))))))


;;;
;;; FEATURES
;;;
(defclass model-feature ()
  ((id :initarg :id :initform (error ":type missing")  :reader id-of)
   (type :initarg :type :initform (error ":type missing") :reader feature-type-of)
   (stroke-paint  :initarg :stroke-paint :initform nil :reader stroke-paint-of)
   (stroke-width  :initarg :stroke-width :initform nil :reader stroke-width-of)
   (fill-paint :initarg :fill-paint :initform nil :reader fill-paint-of)))


;;;
;;;
;;;
(defclass line-feature (model-feature)
  ((origin :initarg :origin :reader origin-of)
   (end :initarg :end :reader end-of)))


;;;
;;; RECTANGLE
;;;
(defclass rect-feature (model-feature)
  ((origin :initarg :origin :reader origin-of)
   (width :initarg :width :reader width-of)
   (height :initarg :height :reader height-of)))


;;;
;;; ELLIPSE
;;;
(defclass ellipse-feature (model-feature)
  ((origin :initarg :origin :reader origin-of)
   (x-radius :initarg :x-radius :reader x-radius-of)
   (y-radius :initarg :y-radius :reader y-radius-of)
   (points :initarg :points :reader points-of)))


;;;
;;; CIRCLE
;;;
(defclass circle-feature (model-feature)
  ((origin :initarg :origin :reader origin-of)
   (radius :initarg :radius :reader radius-of)
   (points :initarg :points :reader points-of)))


;;;
;;; PATH
;;;
(defclass path-feature (model-feature)
  ((points :initarg :points :reader points-of)))


;;;
;;; MODEL
;;;
(defclass model ()
  ((feature-table :initform (make-hash-table :test #'equal))))


(defmethod initialize-instance :after ((this model) &key features)
  (with-slots (feature-table) this
    (loop for feature in features
          do (alexandria:if-let ((id (id-of feature)))
               (setf (gethash id feature-table) feature)
               (push feature (gethash nil feature-table))))))


(defun model-features (model)
  (with-slots (feature-table stray-feature-list) model
    (loop for feature being the hash-value of feature-table
          nconc (alexandria:ensure-list feature))))


(defun find-model-feature-by-id (model id)
  (with-slots (feature-table) model
    (gethash id feature-table)))


(defun invert-model-y (number)
  (- *model-height* number))


(defun extract-points (object)
  (loop for (x y) across (getf object :point-data)
        collect (gamekit:vec2 x (invert-model-y y))))


(defun parse-feature-type (style)
  (let ((stroke-dasharray (getf style :stroke-dasharray)))
    (if (or (null stroke-dasharray)
            (equalp "" stroke-dasharray)
            (equalp "null" stroke-dasharray)
            (equalp "none" stroke-dasharray))
        :obstacle
        (let* ((pattern (split-sequence:split-sequence #\, stroke-dasharray)))
          (if pattern
              (if (null (third pattern))
                  :background
                  :controller)
              :obstacle)))))


(defgeneric make-model-feature (name id feature-type object &key &allow-other-keys)
  (:method (name id feature-type object &key)
    (error "Unrecognized model ~A: ~A" feature-type name)))


(defmethod make-model-feature ((name (eql :path)) id type object &key stroke stroke-width
                                                                   stroke-opacity)
  (make-instance 'path-feature
                 :id id
                 :type type
                 :stroke-paint (parse-color stroke (parse-number:parse-number stroke-opacity))
                 :stroke-width (and stroke-width (parse-number:parse-number stroke-width))
                 :points (extract-points object)))


(defmethod make-model-feature ((name (eql :rect)) id type object &key fill stroke stroke-width
                                                                   fill-opacity stroke-opacity)
  (destructuring-bind (&key x y width height &allow-other-keys) object
    (make-instance 'rect-feature
                   :id id
                   :type type
                   :fill-paint (parse-color fill (parse-number:parse-number fill-opacity))
                   :stroke-paint (parse-color stroke (parse-number:parse-number stroke-opacity))
                   :stroke-width (and stroke-width (parse-number:parse-number stroke-width))
                   :origin (gamekit:vec2 (parse-number:parse-number x)
                                         (invert-model-y (parse-number:parse-number y)))
                   :width (parse-number:parse-number width)
                   :height (parse-number:parse-number height))))


(defmethod make-model-feature ((name (eql :line)) id type object &key stroke stroke-width
                                                                   stroke-opacity)
  (destructuring-bind (&key x1 y1 x2 y2 &allow-other-keys) object
    (make-instance 'line-feature
                   :id id
                   :type type
                   :stroke-paint (parse-color stroke (parse-number:parse-number stroke-opacity))
                   :stroke-width (and stroke-width (parse-number:parse-number stroke-width))
                   :origin (gamekit:vec2 (parse-number:parse-number x1)
                                         (invert-model-y (parse-number:parse-number y1)))
                   :end (gamekit:vec2 (parse-number:parse-number x2)
                                      (invert-model-y (parse-number:parse-number y2))))))


(defmethod make-model-feature ((name (eql :ellipse)) id type object &key fill stroke stroke-width
                                                                      fill-opacity stroke-opacity)
  (destructuring-bind (&key cx cy rx ry &allow-other-keys) object
    (make-instance 'ellipse-feature
                   :id id
                   :type type
                   :fill-paint (parse-color fill (parse-number:parse-number fill-opacity))
                   :stroke-paint (parse-color stroke (parse-number:parse-number stroke-opacity))
                   :stroke-width (and stroke-width (parse-number:parse-number stroke-width))
                   :origin (gamekit:vec2 (parse-number:parse-number cx)
                                         (invert-model-y (parse-number:parse-number cy)))
                   :points (extract-points object)
                   :x-radius (parse-number:parse-number rx)
                   :y-radius (parse-number:parse-number ry))))


(defmethod make-model-feature ((name (eql :circle)) id type object &key fill stroke stroke-width
                                                                     fill-opacity stroke-opacity)
  (destructuring-bind (&key cx cy r &allow-other-keys) object
    (make-instance 'circle-feature
                   :id id
                   :type type
                   :fill-paint (parse-color fill (parse-number:parse-number fill-opacity))
                   :stroke-paint (parse-color stroke (parse-number:parse-number stroke-opacity))
                   :stroke-width (and stroke-width (parse-number:parse-number stroke-width))
                   :origin (gamekit:vec2 (parse-number:parse-number cx)
                                         (invert-model-y (parse-number:parse-number cy)))
                   :points (extract-points object)
                   :radius (parse-number:parse-number r))))


(defun %make-model-feature (object)
  (let* ((style (extract-style object))
         (feature-type (parse-feature-type style))
         (name (alexandria:switch ((getf object :type) :test #'equal)
                 ("path" :path)
                 ("line" :line)
                 ("rect" :rect)
                 ("ellipse" :ellipse)
                 ("circle" :circle))))
    (apply #'make-model-feature name (getf object :id) feature-type object style)))


(defun extract-style (object)
  (alexandria:when-let ((style (getf object :style)))
    (loop for attrib in (split-sequence:split-sequence #\; style)
          for (name value) = (split-sequence:split-sequence #\: attrib)
          append (list (alexandria:make-keyword (uiop:standard-case-symbol-name name))
                       value))))


(defun make-features (model-descriptor &key height &allow-other-keys)
  (let ((*model-height* (parse-number:parse-number height)))
    (loop for object in model-descriptor
          collect (%make-model-feature object))))


(defun parse-model (svg-string)
  (multiple-value-bind (elements svg-attribs) (svgp:parse-svg-string svg-string)
    (make-instance 'model :features (apply #'make-features elements svg-attribs))))
