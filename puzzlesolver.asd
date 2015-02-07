(asdf:defsystem "puzzlesolver"
  :description "Solves square puzzles."
  :version "1.0"
  :author "Peder O. Klingenberg"
  :license "MIT"
  :components ((:file "packages")
               (:file "pieces" :depends-on ("packages"))
               (:file "data" :depends-on ("packages" "pieces"))
               (:file "board" :depends-on ("packages" "pieces" "data"))
               (:file "solver" :depends-on ("packages" "pieces" "board"))))
