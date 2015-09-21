(in-package :cl-user)
(defpackage cl-oclapi-test
  (:use :cl
        :cl-oclapi
        :prove
        :cffi))
(in-package :cl-oclapi-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-oclapi)' in your Lisp.

(plan 1)

(subtest "platform API"
  (subtest "clGetPlatformIDs"
    (is +cl-invalid-value+ (cl-get-platform-ids 0
                                                (null-pointer)
                                                (null-pointer))))
  (subtest "clGetPlatformInfo"
    (is +cl-invalid-value+ (cl-get-platform-info (null-pointer)
                                                 0
                                                 0
                                                 (null-pointer)
                                                 (null-pointer)))))

(finalize)
