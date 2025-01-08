(in-package :cl-check.bits)

(deftype word (size)
  `(simple-bit-vector ,size))

(declaim (ftype (function ((integer 0)) (word *)) make-word))
(defun make-word (size)
  "Allocate a bit vector that can hold `SIZE' bits."
  (the (word *) (make-array size :element-type 'bit)))

(declaim (ftype (function ((word *)) (integer 0 *)) word-size))
(defun word-size (word)
  "Return the number of bits in `WORD'."
  (array-dimension word 0))

(declaim (ftype (function ((integer 0 *) (integer 0 *)) (word *)) integer-word))
(defun integer-word (size n)
  "Return a new `WORD' with `SIZE' bits and encode as much of `N' as will
fit."
  (let ((buffer (make-word size)))
    (iter
      (for i from 0 below size)
      (setf (aref buffer i)
            (if (> (logand (ash 1 i) n) 0)
                1
                0))
      (finally (return (the (word *) buffer))))))

(declaim (ftype (function ((integer 0 *) (word *)) (word *)) resize))
(defun resize (size word)
  "Return a new `WORD' from `WORD' that is expanded or truncated to `SIZE'."
  (let ((buffer (make-word size)))
    (iter
      (for i from 0 below (min size (word-size word)))
      (setf (aref buffer i)
            (aref word i))
      (finally (return (the (word *) buffer))))))

(declaim (ftype (function ((integer 0 *) t) (word *)) coerce-word))
(defun coerce-word (size n)
  "Create a new `WORD' with `SIZE' by trying to interpret `N' as an integer."
  (etypecase n
    ((word *)
     (resize size n))
    (unsigned-byte
     (integer-word size n))
    (integer
     (let ((word (integer-word size (abs n))))
       (setf (aref word (1- size))
             (if (< n 0) 1 0))
       word))
    (number
     (coerce-word size (floor n)))
    (string
     (coerce-word size (floor (read-from-string n))))))

(declaim (ftype (function ((word *)) (integer 0 *)) word-integer))
(defun word-integer (word)
  "Return `WORD' as an `INTEGER'."
  (let ((size (word-size word))
        (result 0))
    (iter
      (for i from 0 to (- size 1))
      (incf result (if (> (aref word i) 0)
                       (expt 2 i)
                       0))
      (finally (return (the (integer 0 *) result))))))

(declaim (ftype (function ((word *) (integer 0 *)) (word *)) lshift))
(defun lshift (word count)
  "Perform a bit-wise left shift of `WORD' by `COUNT'."
  (let* ((size (word-size word))
         (buffer (make-word size)))
    (iter
      (for i from (1- size) downto count)
      (setf (aref buffer i)
            (aref word (- i count)))
      (finally (return (the (word *) buffer))))))

(declaim (ftype (function ((word *) (integer 0 *)) (word *)) rshift))
(defun rshift (word count)
  "Perform a bit-wise right shift of `WORD' by `COUNT'."
  (let* ((size (word-size word))
         (buffer (make-word size)))
    (iter
      (for i from 0 below (- size count))
      (setf (aref buffer i)
            (aref word (+ i count)))
      (finally (return (the (word *) buffer))))))

(declaim (ftype (function ((word *) (word *)) (values (word *) (word *))) upcast))
(defun upcast (word1 word2)
  "Return two `WORD's that are the same as `WORD1' and `WORD2' but are
the same size. If both `WORD1' and `WORD2' are already the same size,
then they are returned; otherwise, a copy of the smaller word is made
and adjusted to be the size of the larger one."
  (let ((size1 (word-size word1))
        (size2 (word-size word2)))
    (cond
      ((= size1 size2)
       (values word1 word2))
      ((> size1 size2)
       (values word1 (resize size1 word2)))
      (t
       (values (resize size2 word1) word2)))))

(declaim (ftype (function ((word *)) boolean) word-zero-p))
(defun word-zero-p (word)
  "Return non-nil when `WORD' is not equal to 0."
  (not
   (iter
     (for bit in-vector word)
     (finding bit such-that (> bit 0)))))

(declaim (ftype (function ((word *) (word *)) (word *)) ^))
(defun ^ (word1 word2)
  "Return the result of a bit-wise exclusive or on `WORD1' and `WORD2'.
The result will be the size of the larger word."
  (multiple-value-bind (new-word1 new-word2) (upcast word1 word2)
    (let ((result (make-word (word-size new-word1))))
      (bit-xor new-word1 new-word2 result))))

(declaim (ftype (function ((word *) (word *)) (word *)) &))
(defun & (word1 word2)
  "Return the result of a bit-wise and on `WORD1' and `WORD2'.
The result will be the size of the larger word."
  (multiple-value-bind (new-word1 new-word2) (upcast word1 word2)
    (let ((result (make-word (word-size new-word1))))
      (bit-and new-word1 new-word2 result))))

(declaim (ftype (function ((word *) (word *)) (word *)) add))
(defun add (word1 word2)
  "Return the result of adding `WORD1' and `WORD2'. The result will be
the size of the larger word."
  (cond
    ((word-zero-p word1)
     word2)
    ((word-zero-p word2)
     word1)
    (t
     (let ((carry (lshift (& word1 word2) 1))
           (base (^ word1 word2)))
       (add base carry)))))

(declaim (ftype (function ((word *) (word *)) (word *)) mult))
(defun mult (word1 word2)
  "Return the result of multiplying `WORD1' and `WORD2'. The result will be
the size of the larger word."
  (labels ((reducer (a b acc)
             (if (word-zero-p b)
                 acc
                 (reducer (lshift a 1)
                          (rshift b 1)
                          (if (> (aref b 0) 0)
                              (add acc a)
                              acc)))))
    (multiple-value-bind (new-word1 new-word2) (upcast word1 word2)
      (let ((result (make-word (word-size new-word1))))
        (if (or (word-zero-p new-word1)
                (word-zero-p new-word2))
            result
            (reducer new-word1 new-word2 result))))))
