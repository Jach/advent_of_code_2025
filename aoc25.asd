(defsystem "aoc25"
  :author "Jach"
  :license "Public Domain/Unlicense"

  :depends-on ("cl-ppcre" "alexandria" "let-plus" "str"
               "coalton" "lparallel" "lgame" "cmu-infix" "serapeum")

  :serial t
  :components ((:file "package")
               (:file "puzzle-inputs")
               (:file "coalton-main")
               (:file "union-find")
               (:file "main")))
