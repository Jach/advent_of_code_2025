(defsystem "aoc25"
  :author "Jach"
  :license "Public Domain/Unlicense"

  :depends-on ("cl-ppcre" "alexandria" "let-plus" "str"
               "coalton")

  :serial t
  :components ((:file "package")
               (:file "puzzle-inputs")
               (:file "coalton-main")
               (:file "main")))
