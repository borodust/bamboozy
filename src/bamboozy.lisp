(cl:in-package :bamboozy)


(fistmage:defgame (bamboozy start-state)
  (:viewport-title "BAMBOOZY")
  (:viewport-width 1024)
  (:viewport-height 768))



(defun play ()
  (fistmage:start 'bamboozy))
