;;;; package.lisp

(defpackage :cl-check
  (:use #:cl))

(defpackage :cl-check.random
  (:use #:cl)
  (:local-nicknames (:t :transducers))
  (:export #:make-splittable-random
           #:next-fixnum
           #:next-double
           #:split
           #:split-n
           #:next-word64
           #:next-word32
           #:next-double
           #:next-float))
