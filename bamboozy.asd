(asdf:defsystem :bamboozy
  :description "Autumn Lisp Game Jam 2018 Entry"
  :license "GPLv3"
  :author "Pavel Korolev"
  :version "1.0.0"
  :depends-on (cl-bodge/physics cl-bodge/physics/2d fistmage)
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:module state
                :serial t
                :components ((:file "start")))
               (:file "bamboozy")))
