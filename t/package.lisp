(defpackage :cl-check-tests
  (:use :cl :iterate :fiveam)
  (:local-nicknames (:random :cl-check.random)
                    (:bits :cl-check.bits))
  (:export :run!
           :all-tests))
