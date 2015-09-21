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
  (with-foreign-objects ((platforms 'cl-platform-id)
                         (num-platforms 'cl-uint))
    (subtest "clGetPlatformIDs"
      (is +cl-invalid-value+ (cl-get-platform-ids 0
                                                  (null-pointer)
                                                  (null-pointer)))
      (is +cl-success+ (cl-get-platform-ids 1 platforms num-platforms))
      (ok (> (mem-aref num-platforms 'cl-uint) 0)))
    (subtest "clGetPlatformInfo"
      (is +cl-invalid-value+ (cl-get-platform-info (null-pointer)
                                                   0
                                                   0
                                                   (null-pointer)
                                                   (null-pointer))))))

(finalize)
