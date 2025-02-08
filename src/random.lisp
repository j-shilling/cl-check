(in-package :cl-check.random)

;;; Types

(deftype word64 ()
  '(bits:word 64))

(deftype seed ()
  'word64)

(deftype gamma ()
  'word64)

;;; Magic Constants

;; These mixer constants can be found here:
;;
;; http://zimbry.blogspot.com/2011/09/better-bit-mixing-improving-on.html
;;
;; This site lays out different variants of the same algorithm that
;; plug in different magic constants. Each variant has three bit shift
;; counts and two multipliers.

;; murmur has 13 mixer

(defmacro defconstant-once (name value &optional (doc nil))
  "Define a global constant, only if it has not already been defined."
  (let ((n name)
        (v value)
        (d doc))
    `(defconstant ,n
       (if (boundp ',n)
           ,n
           ,v)
       ,d)))

(defconstant-once +murmur-hash-3-shift-count-1+ 33)
(defconstant-once +murmur-hash-3-multiplier-1+
  (bits:integer-word 64 #xff51afd7ed558ccd))
(defconstant-once +murmur-hash-3-shift-count-2+ 33)
(defconstant-once +murmur-hash-3-multiplier-2+
    (bits:integer-word 64 #xc4ceb9fe1a85ec53))
(defconstant-once +murmur-hash-3-shift-count-3+ 33)

;; Variant 13

(defconstant-once +murmur-hash-3-variant-13-shift-count-1+ 30)
(defconstant-once +murmur-hash-3-variant-13-multiplier-1+ (bits:integer-word 64 #xbf58476d1ce4e5b9))
(defconstant-once +murmur-hash-3-variant-13-shift-count-2+ 27)
(defconstant-once +murmur-hash-3-variant-13-multiplier-2+ (bits:integer-word 64 #x94d049bb133111eb))
(defconstant-once +murmur-hash-3-variant-13-shift-count-3+ 31)

;;; Hashing Functions

(declaim (ftype (function ((integer 0 *) word64)  word64) shift-xor))
(defun shift-xor (count word)
  "Return a new bit array holding the result of shifting the bits of
`WORD' to the right by `COUNT' bits, then performing a bit-wise `XOR'
with the original `WORD'."
  (bits:^ word (bits:rshift word count)))

(declaim (ftype (function ((integer 0 *) word64 word64)  word64) shift-xor-multiply))
(defun shift-xor-multiply (count multiplier word)
  "Return the result of applying `SHIFT-XOR' on `COUNT' and `WORD', then
multiplying the result by `MULTIPLIER'."
  (bits:mult
   multiplier
   (shift-xor count word)))

(defmacro defmixer (name shift-count-1 multiplier-1 shift-count-2 multiplier-2 shift-count-3)
  `(defun ,name (word)
     "Use the bit mixing from the MurmurHash3 hashing function to scramble up `WORD'."
     (let* ((z1 (shift-xor-multiply ,shift-count-1 ,multiplier-1 word))
            (z2 (shift-xor-multiply ,shift-count-2 ,multiplier-2 z1)))
       (shift-xor ,shift-count-3 z2))))

(declaim (ftype (function (word64) word64) mix-64))
(defmixer mix-64
  +murmur-hash-3-shift-count-1+
  +murmur-hash-3-multiplier-1+
  +murmur-hash-3-shift-count-2+
  +murmur-hash-3-multiplier-2+
  +murmur-hash-3-shift-count-3+)

(declaim (ftype (function (word64) word64) mix-64-variant-13))
(defmixer mix-64-variant-13
  +murmur-hash-3-variant-13-shift-count-1+
  +murmur-hash-3-variant-13-multiplier-1+
  +murmur-hash-3-variant-13-shift-count-2+
  +murmur-hash-3-variant-13-multiplier-2+
  +murmur-hash-3-variant-13-shift-count-3+)

(defun mix-gamma (word)
  (labels ((make-odd (w)
             (bits:ior w (bits:integer-word 64 1)))
           (pop-count (w)
             (iter
               (with result = 0)
               (for i from 0 below 64)
               (incf result (if (> (aref w i) 0) 1 0))
               (finally (return result)))))
    (let* ((z1 (mix-64-variant-13 word))
           (z2 (make-odd z1))
           (z3 (bits:^ z2 (bits:rshift z2 1))))
      (if (>= (pop-count z3) 24)
          z2
          (bits:^ z2 (bits:integer-word #xaaaaaaaaaaaaaaaa 64))))))

(defconstant-once +single-float-ulp+
    (the single-float (/ 1.0 (bits:word-single-float (bits:lshift (bits:integer-word 32 1) 24)))))
(defconstant-once +double-float-ulp+
    (the double-float (/ 1.0 (bits:word-double-float (bits:lshift (bits:integer-word 64 1) 53)))))

;;; Splittable Random State

(defconstant-once +golden-gamma+
    (bits:integer-word 64 #x9e3779b97f4a7c15))

(defstruct (splittable-random
            (:constructor %make-splittable-random))
  (seed (error "Missing seed") :read-only t :type seed)
  (gamma (error "Missing gamma") :read-only t :type gamma))

(declaim (ftype (function (&optional (or integer seed null) (or integer gamma null)) splittable-random) make-splittable-random))
(defun make-splittable-random (&optional (seed nil) (gamma nil))
  (let* ((s (if seed
                (bits:coerce-word 64 seed)
                (bits:integer-word 64 (random (expt 2 64)))))
         (g (if gamma
                (bits:coerce-word 64 gamma)
                (mix-64-variant-13 (bits:add s +golden-gamma+)))))
    (%make-splittable-random :seed s
                             :gamma g)))

(defun next-seed (rnd)
  (bits:add (splittable-random-seed rnd)
            (splittable-random-gamma rnd)))

;;; Public interface

(defun next-word64 (rnd)
  (bits:word-integer (mix-64 (next-seed rnd))))

(defun next-word32 (rnd)
  (bits:word-integer (bits::resize 32 (mix-64 (next-seed rnd)))))

(defun next-double-float (rnd)
  (let ((word (bits:integer-word 64 (next-word64 rnd))))
    (* (bits:word-double-float (bits:rshift word 11))
       +double-float-ulp+)))

(defun next-single-float (rnd)
  (let ((word (bits:integer-word 32 (next-word32 rnd))))
    (* (bits:word-single-float (bits:rshift word 8))
       +single-float-ulp+)))

(defun split (rnd)
  (let* ((seed (splittable-random-seed rnd))
         (gamma (splittable-random-gamma rnd))
         (seed-1 (bits:add seed gamma))
         (seed-2 (bits:add seed-1 gamma)))
    (values
     (%make-splittable-random :seed seed-2 :gamma gamma)
     (%make-splittable-random :seed (mix-64 seed-1) :gamma (mix-gamma seed-2)))))
