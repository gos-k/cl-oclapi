(in-package :cl-user)
(defpackage cl-oclapi-test
  (:use :cl
        :cl-oclapi
        :prove
        :cffi))
(in-package :cl-oclapi-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-oclapi)' in your Lisp.

(plan 2)

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
                                                   (null-pointer)))
      (cl-get-platform-ids 1 platforms num-platforms)
      (let ((platform (mem-aref platforms 'cl-platform-id)))
        (with-foreign-objects ((param-value 'cl-uchar 256)
                               (param-value-size-ret 'size-t))
          (is +cl-success+ (cl-get-platform-info platform
                                                 +cl-platform-profile+
                                                 256
                                                 param-value
                                                 param-value-size-ret))
          (is "FULL_PROFILE" (foreign-string-to-lisp param-value))
          (is +cl-success+ (cl-get-platform-info platform
                                                 +cl-platform-version+
                                                 256
                                                 param-value
                                                 param-value-size-ret))
          (is "OpenCL 1.2 pocl 0.10" (foreign-string-to-lisp param-value))
          (is +cl-success+ (cl-get-platform-info platform
                                                 +cl-platform-name+
                                                 256
                                                 param-value
                                                 param-value-size-ret))
          (is "Portable Computing Language" (foreign-string-to-lisp param-value))
          (is +cl-success+ (cl-get-platform-info platform
                                                 +cl-platform-vendor+
                                                 256
                                                 param-value
                                                 param-value-size-ret))
          (is "The pocl project" (foreign-string-to-lisp param-value))
          (is +cl-success+ (cl-get-platform-info platform
                                                 +cl-platform-extensions+
                                                 256
                                                 param-value
                                                 param-value-size-ret))
          (is "cl_khr_icd" (foreign-string-to-lisp param-value)))))))

(subtest "Device API"
  (with-foreign-objects ((platforms 'cl-platform-id)
                         (num-platforms 'cl-uint)
                         (devices 'cl-device-id)
                         (num-devices 'cl-uint))
    (cl-get-platform-ids 1 platforms num-platforms)
    (let ((platform (mem-aref platforms 'cl-platform-id)))
      (subtest "clGetDeviceIDs"
        (is +cl-invalid-value+ (cl-get-device-ids (null-pointer)
                                                  0
                                                  0
                                                  (null-pointer)
                                                  (null-pointer)))
        (is +cl-success+ (cl-get-device-ids platform
                                            +cl-device-type-cpu+
                                            1
                                            devices
                                            num-devices))
        (ok (> (mem-aref num-devices 'cl-uint) 0)))
      (subtest "clGetDeviceInfo"
        (is +cl-invalid-device+ (cl-get-device-info (null-pointer)
                                                    0
                                                    0
                                                    (null-pointer)
                                                    (null-pointer))))
      (subtest "clCreateSubDevices"
        (is +cl-invalid-device+ (cl-create-sub-devices (null-pointer)
                                                       (null-pointer)
                                                       0
                                                       (null-pointer)
                                                       (null-pointer))))
      (subtest "clRetainDevice"
        (is +cl-invalid-device+ (cl-retain-device (null-pointer))))
      (subtest "clReleaseDevice"
        (is +cl-invalid-device+ (cl-release-device (null-pointer)))))))

(finalize)
