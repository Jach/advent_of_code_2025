(defsystem "aoc25"
  :author "Jach"
  :license "Public Domain/Unlicense"

  :depends-on ("cl-ppcre" "alexandria" "let-plus" "str"
               "lparallel" "lgame" "cmu-infix" "serapeum"
               "function-cache")

  :serial t
  :components ((:file "package")
               (:file "puzzle-inputs")
               (:file "union-find")
               (:file "main")))
