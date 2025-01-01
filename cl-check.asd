(asdf:defsystem :cl-check
  :description "A QuickCheck-style property base testing framework"
  :author "Jake Shilling"
  :license  "GPL-3-or-later"
  :version "0.0.1"
  :depends-on (:alexandria :transducers)
  :pathname "src/"
  :components ((:file "package")
               (:file "random"))
  :in-order-to ((asdf:test-op (asdf:test-op :cl-check/test))))

(asdf:defsystem :cl-check/test
  :description "Tests for cl-check"
  :author "Jake Shilling"
  :license  "GPL-3-or-later"
  :depends-on (:cl-check :fiveam)
  :pathname "t/"
  :components ((:file "package")
               (:file "tests" :depends-on ("package")))
  :perform (asdf:test-op (o c) (symbol-call :fiveam :run! :all-tests)))
