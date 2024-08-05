(asdf:defsystem "aoc-2023"
  :description "System for completing the 2023 Advent of Code problems"
  :depends-on ("alexandria"
               "arrow-macros"
               "str"
               "uiop")
  :components ((:file "utils-packed")))
