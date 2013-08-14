;;;; hand-evaluator.asd

(asdf:defsystem #:hand-evaluator
  :serial t
  :description "Describe hand-evaluator here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cl-unit-tests
	       #:cl-cards)
  :components ((:file "package")
               (:file "hand-evaluator")
	       (:file "tests")))
