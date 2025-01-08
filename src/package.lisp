;;;; package.lisp

(defpackage :cl-check
  (:use #:cl))

(defpackage :cl-check.bits
  (:use :cl :iterate)
  (:export #:word
           #:integer-word
           #:word-integer
           #:word-size
           #:lshift
           #:rshift
           #:upcast
           #:^
           #:&
           #:add
           #:mult))

(defpackage :cl-check.random
  (:use :cl)
  (:local-nicknames (:t :transducers)
                    (:bits :cl-check.bits))
  (:export #:make-splittable-random
           #:next-fixnum
           #:next-double
           #:split
           #:split-n
           #:next-word64
           #:next-word32
           #:next-double
           #:next-float))
