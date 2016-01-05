;;;; bosom-serpent.asd

(asdf:defsystem #:bosom-serpent
  :description "Describe bosom-serpent here"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :defsystem-depends-on (:asdf-package-system)
  :class :package-inferred-system
  :depends-on (#:bosom-serpent/all))

(register-system-packages
 "burgled-batteries"
 '(:burgled-batteries :python.cffi))

