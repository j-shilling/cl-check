;;;; package.lisp

(defpackage :cl-check
  (:use #:cl))

(defpackage :cl-check.bits
  (:use :cl :iterate)
  (:export #:word
           #:resize
           #:integer-word
           #:coerce-word
           #:word-integer
           #:word-single-float
           #:word-double-float
           #:word-size
           #:lshift
           #:rshift
           #:upcast
           #:^
           #:ior
           #:&
           #:add
           #:mult))

(defpackage :cl-check.random
  (:use :cl :iterate)
  (:local-nicknames (:t :transducers)
                    (:bits :cl-check.bits))
  (:export #:make-splittable-random
           #:next-fixnum
           #:next-double
           #:split
           #:split-n
           #:next-word64
           #:next-word32
           #:next-double-float
           #:next-single-float))
