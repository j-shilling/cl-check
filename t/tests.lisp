(in-package :cl-check-tests)

(def-suite :all-tests
  :description "All cl-check tests")

(in-suite :all-tests)

(def-suite :bits
  :description "Bit-wise operations"
  :in :all-tests)

(in-suite :bits)

(test create-word-32
  (for-all ((n (gen-integer :max (1- (expt 2 32)) :min 0)))
    (let* ((as-word (bits:integer-word 32 n))
           (as-integer (bits:word-integer as-word)))
      (is (= n as-integer)))))

(test create-word-64
  (for-all ((n (gen-integer :max (1- (expt 2 64)) :min 0)))
    (let* ((as-word (bits:integer-word 64 n))
           (as-integer (bits:word-integer as-word)))
      (is (= n as-integer)))))

(test coerce-word-from-word
  (for-all ((n (gen-integer :max (1- (expt 2 64)) :min 0))
            (size1 (gen-one-element 32 64))
            (size2 (gen-one-element 32 64)))
    (let* ((input (bits:integer-word size1 n))
           (expected (bits::resize size2 (bits:integer-word size1 n)))
           (actual (bits:coerce-word size2 input)))
      (is (equal expected actual)))))

(test coerce-word-from-integer
  (for-all ((n (gen-integer :max (1- (expt 2 64)) :min 0))
            (size (gen-one-element 32 64)))
    (let* ((expected (bits:integer-word size n))
           (actual (bits:coerce-word size n)))
      (is (equal expected actual)))))

(test coerce-word-from-real
  (for-all ((n (gen-float))
            (size (gen-one-element 32 64)))
    (let* ((expected (bits:integer-word size (floor (abs n))))
           (actual (bits:coerce-word size (abs n))))
      (is (equal expected actual)))))

(test coerce-word-from-string-integer
  (for-all ((n (gen-integer :max (1- (expt 2 64)) :min 0))
            (size (gen-one-element 32 64)))
    (let* ((expected (bits:integer-word size n))
           (actual (bits:coerce-word size (write-to-string n))))
      (is (equal expected actual)))))

(test coerce-word-from-string-real
  (for-all ((n (gen-float))
            (size (gen-one-element 32 64)))
    (let* ((expected (bits:integer-word size (floor (abs n))))
           (actual (bits:coerce-word size (write-to-string (abs n)))))
      (is (equal expected actual)))))

(test lshift-word-32
  (for-all ((count (gen-integer :max 32 :min 0))
            (n (gen-integer :max (1- (expt 2 32)) :min 0)))
    (let* ((expected (bits:integer-word 32 (* n (expt 2 count))))
           (actual (bits:lshift (bits:integer-word 32 n) count)))
      (is (equal expected actual)))))

(test lshift-word-64
  (for-all ((count (gen-integer :max 64 :min 0))
            (n (gen-integer :max (1- (expt 2 64)) :min 0)))
    (let* ((expected (bits:integer-word 64 (* n (expt 2 count))))
           (actual (bits:lshift (bits:integer-word 64 n) count)))
      (is (equal expected actual)))))

(test rshift-word-32
  (for-all ((count (gen-integer :max 32 :min 0))
            (n (gen-integer :max (1- (expt 2 32)) :min 0)))
    (let* ((expected (bits:integer-word 32 (floor (/ n (expt 2 count)))))
           (actual (bits:rshift (bits:integer-word 32 n) count)))
      (is (equal expected actual)))))

(test rshift-word-64
  (for-all ((count (gen-integer :max 64 :min 0))
            (n (gen-integer :max (1- (expt 2 64)) :min 0)))
    (let* ((expected (bits:integer-word 64 (floor (/ n (expt 2 count)))))
           (actual (bits:rshift (bits:integer-word 64 n) count)))
      (is (equal expected actual)))))

(test upcast
  (for-all ((n1 (gen-integer :max (1- (expt 2 32)) :min 0))
            (n2 (gen-integer :max (1- (expt 2 32)) :min 0))
            (size1 (gen-one-element 32 64))
            (size2 (gen-one-element 32 64)))
    (let* ((word1 (bits:integer-word size1 n1))
           (word2 (bits:integer-word size2 n2)))
      (multiple-value-bind (new-word-1 new-word-2) (bits:upcast word1 word2)
        (is (= n1 (bits:word-integer word1)))
        (is (= n2 (bits:word-integer word2)))
        (is (= (bits:word-size new-word-1)
               (bits:word-size new-word-2)))))))

(test xor
  (for-all ((n1 (gen-integer :max (1- (expt 2 32)) :min 0))
            (n2 (gen-integer :max (1- (expt 2 32)) :min 0))
            (size1 (gen-one-element 32 64))
            (size2 (gen-one-element 32 64)))
    (let* ((word1 (bits:integer-word size1 n1))
           (word2 (bits:integer-word size2 n2))
           (result (bits:^ word1 word2))
           (expected (bits::make-word (max (bits:word-size word1)
                                     (bits:word-size word2)))))
      (iter
        (for i from 0 below 32)
        (setf (aref expected i)
              (if (or
                   (and (= (aref word1 i) 0)
                        (= (aref word2 i) 0))
                   (and (> (aref word1 i) 0)
                        (> (aref word2 i) 0)))
                  0
                  1)))
      (is (= (bits:word-size result)
             (max (bits:word-size word1)
                  (bits:word-size word2))))
      (is (equal expected result)))))

(test and
  (for-all ((n1 (gen-integer :max (1- (expt 2 32)) :min 0))
            (n2 (gen-integer :max (1- (expt 2 32)) :min 0))
            (size1 (gen-one-element 32 64))
            (size2 (gen-one-element 32 64)))
    (let* ((word1 (bits:integer-word size1 n1))
           (word2 (bits:integer-word size2 n2))
           (result (bits:& word1 word2))
           (expected (bits::make-word (max (bits:word-size word1)
                                     (bits:word-size word2)))))
      (iter
        (for i from 0 below 32)
        (setf (aref expected i)
              (if (and (> (aref word1 i) 0)
                        (> (aref word2 i) 0))
                  1
                  0)))
      (is (= (bits:word-size result)
             (max (bits:word-size word1)
                  (bits:word-size word2))))
      (is (equal expected result)))))

(test add
  (for-all ((n1 (gen-integer :max (1- (expt 2 32)) :min 0))
            (n2 (gen-integer :max (1- (expt 2 32)) :min 0))
            (size1 (gen-one-element 32 64))
            (size2 (gen-one-element 32 64)))
    (let* ((word1 (bits:integer-word size1 n1))
           (word2 (bits:integer-word size2 n2))
           (result (bits:add word1 word2))
           (expected (bits:integer-word (max (bits:word-size word1)
                                             (bits:word-size word2))
                                        (+ n1 n2))))
      (is (equal expected result)))))

(test mult
  (for-all ((n1 (gen-integer :max (1- (expt 2 32)) :min 0))
            (n2 (gen-integer :max (1- (expt 2 32)) :min 0))
            (size1 (gen-one-element 32 64))
            (size2 (gen-one-element 32 64)))
    (let* ((word1 (bits:integer-word size1 n1))
           (word2 (bits:integer-word size2 n2))
           (result (bits:mult word1 word2))
           (expected (bits:integer-word (max (bits:word-size word1)
                                             (bits:word-size word2))
                                        (* n1 n2))))
      (is (equal expected result)))))

(def-suite :random
  :description "Splittable Random Number Generator"
  :in :all-tests)

(in-suite :random)

(defun load-test-data ()
  (let* ((csv (cl-csv:read-csv #P"./t/test-data.csv"))
         (header (car csv))
         (rows (cdr csv)))
    (mapcar (lambda (row)
              (loop for datum in row
                    for label in header
                    collect (cons (intern (string-upcase label) :keyword) datum)))
            rows)))

(defparameter *test-data*
  (load-test-data))

(test next-word64
  (for-all ((test-data (apply #'gen-one-element *test-data*)))
    (let ((seed (bits:coerce-word 64 (cdr (assoc :seed test-data))))
          (gamma (bits:coerce-word 64 (cdr (assoc :gamma test-data))))
          (expected-word64 (bits:word-integer (bits:coerce-word 64 (cdr (assoc :randword64 test-data))))))
      (let* ((rnd (random:make-splittable-random seed gamma))
             (actual-word64 (random:next-word64 rnd)))
        (is (= expected-word64 actual-word64))))))

(test next-word32
  (for-all ((test-data (apply #'gen-one-element *test-data*)))
    (let ((seed (bits:coerce-word 64 (cdr (assoc :seed test-data))))
          (gamma (bits:coerce-word 64 (cdr (assoc :gamma test-data))))
          (expected-word32 (bits:word-integer (bits:coerce-word 32 (cdr (assoc :randword64 test-data))))))
      (let* ((rnd (random:make-splittable-random seed gamma))
             (actual-word32 (random:next-word32 rnd)))
        (is (= expected-word32 actual-word32))))))

(test next-double-float
  (for-all ((test-data (apply #'gen-one-element *test-data*)))
    (let ((seed (bits:coerce-word 64 (cdr (assoc :seed test-data))))
          (gamma (bits:coerce-word 64 (cdr (assoc :gamma test-data))))
          (expected-double (let ((*read-default-float-format* 'double-float))
                             (read-from-string (cdr (assoc :randdouble test-data))))))
      (let* ((rnd (random:make-splittable-random seed gamma))
             (actual-double (random:next-double-float rnd)))
        (is (= expected-double actual-double))))))

(test next-single-float
  (for-all ((test-data (apply #'gen-one-element *test-data*)))
    (let ((seed (bits:coerce-word 64 (cdr (assoc :seed test-data))))
          (gamma (bits:coerce-word 64 (cdr (assoc :gamma test-data))))
          (expected-float (let ((*read-default-float-format* 'single-float))
                            (read-from-string (cdr (assoc :randfloat test-data))))))
      (let* ((rnd (random:make-splittable-random seed gamma))
             (actual-float (random:next-single-float rnd)))
        (is (= expected-float actual-float))))))

(test split-seed
  (for-all ((test-data (apply #'gen-one-element *test-data*)))
    (let ((seed (bits:coerce-word 64 (cdr (assoc :seed test-data))))
          (gamma (bits:coerce-word 64 (cdr (assoc :gamma test-data))))
          (expected-split-seed-1 (read-from-string (cdr (assoc :splitTupleFirstSeed test-data))))
          (expected-split-seed-2 (read-from-string (cdr (assoc :splitTupleSecondSeed test-data)))))
      (let* ((rnd (random:make-splittable-random seed gamma))
             (next-rnds (multiple-value-list (random:split rnd)))
             (actual-split-seed-1  (bits:word-integer (random::splittable-random-seed (car next-rnds))))
             (actual-split-seed-2  (bits:word-integer (random::splittable-random-seed (cadr next-rnds)))))
        (is (= expected-split-seed-1 actual-split-seed-1)
            (= expected-split-seed-2 actual-split-seed-2))))))

(test split-gamma
  (for-all ((test-data (apply #'gen-one-element *test-data*)))
    (let ((seed (bits:coerce-word 64 (cdr (assoc :seed test-data))))
          (gamma (bits:coerce-word 64 (cdr (assoc :gamma test-data))))
          (expected-split-gamma-1 (read-from-string (cdr (assoc :splitTupleFirstGamma test-data))))
          (expected-split-gamma-2 (read-from-string (cdr (assoc :splitTupleSecondGamma test-data)))))
      (let* ((rnd (random:make-splittable-random seed gamma))
             (next-rnds (multiple-value-list (random:split rnd)))
             (actual-split-gamma-1 (bits:word-integer (random::splittable-random-gamma (car next-rnds))))
             (actual-split-gamma-2 (bits:word-integer (random::splittable-random-gamma (cadr next-rnds)))))
        (is (= expected-split-gamma-1 actual-split-gamma-1)
            (= expected-split-gamma-2 actual-split-gamma-2))))))
