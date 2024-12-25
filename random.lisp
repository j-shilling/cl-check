;;;

(defconstant +phi+
  (/ (+ 1 (sqrt 5)) 2))

(defconstant +fixnum-length+
  (integer-length most-negative-fixnum))

(defconstant +golden-gamma+
  (floor (/ (expt 2 +fixnum-length+)
            +phi+)))

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

(defun add-fixnum (fx1 fx2)
  "Add `FX1' and `FX2', then perform a `LOGAND' to truncate the result to the size of a `FIXNUM'."
  (logand most-positive-fixnum (+ fx1 fx2)))

(defun mix-fixnum (n)
  (declare (type fixnum n))
  (let* ((step1 (* (logxor n (ash n -30)) +mix-constant-1+))
         (step2 (* (logxor step1 (ash step1 -27)) +mix-constant-2+))
         (step3 (logxor step2 (ash step2 -31))))
    (logand most-positive-fixnum step3)))

(defun mix-gamma (n)
  (declare (type fixnum n))
  (let* ((step1 (* (logxor n (ash n -33)) +gamma-mix-constant-1+))
         (step2 (* (logxor step1 (ash step1 -33)) +gamma-mix-constant-2+))
         (step3 (logior (logxor step2 (ash step2 -33)) 1)))
    (logand most-positive-fixnum
     (if (< (integer-length (logxor step3 (ash step3 -1))) 24)
         (logxor step3 #xaaaaaaaaaaaaaaaa)
         step3))))

(defstruct (splittable-random (:constructor internal-make-splittable-random))
  (seed (default-random) :read-only t :type fixnum)
  (gamma +golden-gamma+ :read-only t :type fixnum))

(defun make-splittable-random (&optional (seed nil))
  (if seed
      (progn
        (declare (type fixnum seed))
        (internal-make-splittable-random :seed seed :gamma +golden-gamma+))
      (let ((s (add-fixnum (default-random)
                           (* 2 +golden-gamma+))))
        (internal-make-splittable-random :seed (mix-fixnum s)
                                         :gamma (mix-gamma (add-fixnum s +golden-gamma+))))))

(defun splittable-random-next-seed (rng)
  (declare (type splittable-random rng))
  (add-fixnum (splittable-random-seed rng)
              (splittable-random-gamma rng)))

(defun splittable-random-next-fixnum (rng)
  (declare (type splittable-random rng))
  (mix-fixnum (next-seed rng)))

(defun splittable-randoms-plit (rng)
  (declare (type splittable-random rng))
  (make-splittable-random :seed (next-fixnum rng)
                          :gamma (mix-gamma (next-fixnum rng))))
