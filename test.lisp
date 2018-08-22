(defpackage :bosom-serpent/test
  (:use :cl :FiveAM :serapeum)
  (:export :run-tests))
(in-package :bosom-serpent/test)

(defun run-tests ()
  (5am:run! 'bosom-serpent))

(def-suite bosom-serpent)
(in-suite bosom-serpent)

(vernacular:import shlex
  :as :bosom-serpent/python2
  :from "shlex_stub.py"
  :binding (#'lex))

(test lex
  (is (seq=
       (lex "how now 'brown cow'")
       #("how" "now" "brown cow"))))
