(in-package :cl-check-tests)

(def-suite :all-tests
  :description "All cl-check tests")

(in-suite :all-tests)

(defun load-test-data ()
  (let* ((csv (cl-csv:read-csv #P"test-data.csv"))
         (header (car csv))
         (rows (cdr csv)))
    (mapcar (lambda (row)
              (loop for datum in row
                    for label in header
                    collect (cons (intern (string-upcase label) :keyword) datum)))
            rows)))

(defparameter *test-data*
  (load-test-data))

(test splittable-random
  (for-all ((test-data (apply #'gen-one-element *test-data*)))
    (let ((seed (read-from-string (cdr (assoc :seed test-data))))
          (gamma (read-from-string (cdr (assoc :gamma test-data))))
          (expected-word64 (read-from-string(cdr (assoc :randword64 test-data))))
          ;; (expected-word32 (read-from-string(cdr (assoc :randword32 test-data))))
          ;; (expected-double (read-from-string(cdr (assoc :randdouble test-data))))
          ;; (expected-float (read-from-string(cdr (assoc :randfloat test-data))))
          )
      (let* ((rnd (random:make-splittable-random seed gamma))
             (actual-word64 (random:next-word64 rnd)))
        (is (= expected-word64 actual-word64))))))
