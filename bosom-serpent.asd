;;;; bosom-serpent.asd

(asdf:defsystem #:bosom-serpent
  :description "Import Python modules as Overlord modules"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :defsystem-depends-on (:asdf-package-system)
  :class :package-inferred-system
  :depends-on (#:bosom-serpent/all))

(asdf:register-system-packages
 "burgled-batteries"
 '(:burgled-batteries :python.cffi))
