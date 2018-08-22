;;;; bosom-serpent.asd

(defsystem "bosom-serpent"
  :description "Import Python modules as Vernacular modules"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :defsystem-depends-on (:asdf-package-system)
  :class :package-inferred-system
  :depends-on ("bosom-serpent/all")
  :in-order-to ((test-op (test-op "bosom-serpent/test")))
  :perform (test-op (o c) (symbol-call :bosom-serpent/test :run-tests)))

(register-system-packages
 "burgled-batteries"
 '(:burgled-batteries :python.cffi))
