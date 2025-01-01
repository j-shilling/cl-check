(defpackage #:cl-check.random
  (:use #:cl)
  (:local-nicknames (:t :transducers))
  (:export #:make-splittable-random
           #:next-fixnum
           #:next-double
           #:split
           #:split-n))

(in-package #:cl-check.random)

(defconstant +phi+
  (/ (+ 1 (sqrt 5)) 2)
  "Golden ratio. Used to randomize bits.")

(defconstant +fixnum-length+
  (integer-length most-negative-fixnum)
  "Size of fixnum on this platform")

(defconstant +golden-gamma+
  (floor (/ (expt 2 +fixnum-length+)
            +phi+))
  "The golden ratio scaled to the size of a fixnum")

(defconstant +mix-constant-1+
  #xbf58476d1ce4e5b9)

(defconstant +mix-constant-2+
  #x94d049bb133111eb)

(defconstant +gamma-mix-constant-1+
  #xff51afd7ed558ccd)

(defconstant +gamma-mix-constant-2+
  #xc4ceb9fe1a85ec53)

(defun default-random ()
  (random most-positive-fixnum))

(declaim (ftype (function (integer) fixnum) integer-fixnum))
(defun integer-fixnum (n)
  "Return `N' truncated to a fixnum. The randomization techniques are
qported from languages that assume fix-sized integers and take
advantage of overflow that happens."
  (declare (type integer n))
  (coerce
   (logand most-positive-fixnum n)
   'fixnum))

(declaim (ftype (function (integer integer) fixnum) fx+))
(defun fx+ (a b)
  "Add `A' and `B', then ensures the result is `FIXNUM'."
  (declare (type integer a)
           (type integer b))
  (integer-fixnum (+ a b)))

(declaim (ftype (function (fixnum) fixnum) mix-fixnum))
(defun mix-fixnum (n)
  "Mix the bits in `N' up to product a random `FIXNUM'."
  (declare (type fixnum n))
  (let* ((step1 (* (logxor n (ash n -30)) +mix-constant-1+))
         (step2 (* (logxor step1 (ash step1 -27)) +mix-constant-2+))
         (step3 (logxor step2 (ash step2 -31))))
    (integer-fixnum step3)))

(declaim (ftype (function (fixnum) fixnum) mix-gamma))
(defun mix-gamma (n)
  "Like `MIX-FIXNUM', but for getting the next `GAMMA' value."
  (declare (type fixnum n))
  (let* ((step1 (* (logxor n (ash n -33)) +gamma-mix-constant-1+))
         (step2 (* (logxor step1 (ash step1 -33)) +gamma-mix-constant-2+))
         (step3 (logior (logxor step2 (ash step2 -33)) 1)))
    (integer-fixnum
     (if (< (integer-length (logxor step3 (ash step3 -1))) 24)
         (logxor step3 #xaaaaaaaaaaaaaaaa)
         step3))))

(defstruct (splittable-random (:constructor internal-make-splittable-random))
  (seed (default-random) :read-only t :type fixnum)
  (gamma +golden-gamma+ :read-only t :type fixnum))

(declaim (ftype (function (&optional (or fixnum null)) splittable-random) make-splittable-random))
(defun make-splittable-random (&optional (seed nil))
  "Create a `SPLITTABLE-RANDOM' from `SEED'. Generate a good `SEED' if not provided."
  (if seed
      (progn
        (internal-make-splittable-random :seed seed :gamma +golden-gamma+))
      (let ((s (fx+ (default-random)
                    (* 2 +golden-gamma+))))
        (internal-make-splittable-random :seed (mix-fixnum s)
                                         :gamma (mix-gamma (fx+ s +golden-gamma+))))))

(declaim (ftype (function (splittable-random) fixnum) next-fixnum))
(defun next-fixnum (rng)
  "Draw the next random `FIXNUM' from `RNG'."
  (declare (type splittable-random rng))
  (let ((seed (splittable-random-seed rng))
        (gamma (splittable-random-gamma rng)))
    (mix-fixnum (fx+ seed gamma))))

(declaim (ftype (function (splittable-random) double-float) next-double))
(defun next-double (rng)
  "Draw the next random `DOUBLE-FLOAT' from `RNG'"
  (declare (type splittable-random rng))
  (* double-float-epsilon
     (ash (next-fixnum rng) 11)))

(declaim (ftype (function (splittable-random) list) split))
(defun split (rng)
  "Create to new `SPLITTABLE-RANDOM's that are statistically independent from each other."
  (declare (type splittable-random rng))
  (let* ((seed (splittable-random-seed rng))
         (gamma (splittable-random-gamma rng))
         (seed-1 (fx+ gamma seed))
         (seed-2 (fx+ gamma seed-1))
         (gamma-1 (mix-gamma seed-2)))
    (list
     (internal-make-splittable-random :seed seed-2 :gamma gamma)
     (internal-make-splittable-random :seed (mix-fixnum seed-1) :gamma gamma-1))))

(declaim (ftype (function (splittable-random (integer 0 *)) list) split-n))
(defun split-n (rng n)
  "Return a list of `N' new `SPLITTABLE-RANDOM's that are all independent
from each other."
  (check-type n (integer 0 *))
  (cond
    ((= n 0) nil)
    ((= n 1) (list rng))
    (t
     (let ((gamma (splittable-random-gamma rng))
           (seed (splittable-random-seed *rng*)))
       (labels ((do-split-n (i seed acc)
                  (if (= i 0)
                      (cons (internal-make-splittable-random :seed seed
                                                             :gamma gamma)
                            acc)
                      (let* ((seed-1 (fx+ gamma seed))
                             (seed-2 (fx+ gamma seed-1))
                             (gamma-1 (mix-gamma seed-2))
                             (new-rng (internal-make-splittable-random :seed (mix-fixnum seed-1)
                                                                       :gamma gamma-1)))
                        (do-split-n (- i 1) seed-2 (cons new-rng acc))))))
         (do-split-n (- n 1) seed nil))))))

(declaim (ftype (function (&optional (or splittable-random null)) t::generator) lazy-random-states))
(defun lazy-random-states (&optional (initial-state nil))
  "Create a source that yields infinitely many `SPLITTABLE-RANDOM's, each
independent from the rest. This source can then be passed to
`:TRANSDUCERS/TRANSDUCE' to automate the splitting process.

Ex.

;; (:local-nicknames (:t :transducers))

(t:transduce (t:take 10)
             #'cons
             (lazy-random-states))
;;=> A list with 10 random number generators

(t:transduce (t:comp
               (t:take 10)
               (t:map #'next-fixnum))
             #'cons
             (lazy-random-states))
;;=> A list of 10 fixnums."
  (let* ((rng (or initial-state (make-splittable-random)))
         (func (lambda ()
                 (destructuring-bind (rng-1 rng-2) (split rng)
                   (setf rng rng-2)
                   rng-1))))
    (t::make-generator :func func)))
