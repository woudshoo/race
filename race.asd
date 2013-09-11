;;;; race.asd

(asdf:defsystem #:race
  :serial t
  :description "Describe race here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:wo-util #:alexandria)
  :components ((:file "package")
               (:file "race")))

