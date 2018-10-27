(asdf:defsystem :bamboozy
  :description "Autumn Lisp Game Jam 2018 Entry"
  :license "GPLv3"
  :author "Pavel Korolev"
  :version "1.0.0"
  :depends-on (bodge-utilities alexandria split-sequence parse-number cl-svg-polygon
                               cl-bodge/physics cl-bodge/physics/2d fistmage)
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "game")
               (:file "camera")
               (:file "model")
               (:file "visual")
               (:module objects
                :serial t
                :components ((:file "slime")
                             (:file "level")))
               (:module state
                :serial t
                :components ((:file "start")))
               (:file "bamboozy")))
