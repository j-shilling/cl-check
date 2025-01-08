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

(defconstant +murmur-hash-3-shift-count-1+ 33)
(defconstant +murmur-hash-3-multiplier-1+ (bits:integer-word 64 #xff51afd7ed558ccd))
(defconstant +murmur-hash-3-shift-count-2+ 33)
(defconstant +murmur-hash-3-multiplier-2+ (bits:integer-word 64 #xc4ceb9fe1a85ec53))
(defconstant +murmur-hash-3-shift-count-3+ 33)

;; Variant 13

(defconstant +murmur-hash-3-variant-13-shift-count-1+ 30)
(defconstant +murmur-hash-3-variant-13-multiplier-1+ (bits:integer-word 64 #xbf58476d1ce4e5b9))
(defconstant +murmur-hash-3-variant-13-shift-count-2+ 27)
(defconstant +murmur-hash-3-variant-13-multiplier-2+ (bits:integer-word 64 #x94d049bb133111eb))
(defconstant +murmur-hash-3-variant-13-shift-count-3+ 31)

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

;;; Splittable Random State

(defstruct (splittable-random
            (:constructor %make-splittable-random))
  (seed (error "Missing seed") :read-only t :type seed)
  (gamma (error "Missing gamma") :read-only t :type gamma))

(declaim (ftype (function (&optional (or seed null) (or gamma null)) splittable-random) make-splittable-random))
(defun make-splittable-random (&optional (seed nil) (gamma nil))
  (when (or (not seed)
            (not gamma))
    (error "Not implemented"))
  (let ((s (etypecase seed
             (word64 seed)
             ((integer 0 *) (bits:integer-word 64 seed))))
        (g (etypecase gamma
             (word64 gamma)
             ((integer 0 *) (bits:integer-word 64 gamma)))))
    (%make-splittable-random :seed s :gamma g)))

;;; Public interface

(defun next-word64 (rnd)
  (bits->int (mix-64 (next-seed rnd))))

(defun next-seed (rnd)
  (bit+ (splittable-random-seed rnd)
        (splittable-random-gamma rnd)))
