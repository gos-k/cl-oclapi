(in-package :cl-user)
(defpackage cl-oclapi-test
  (:use :cl
        :cl-oclapi
        :prove
        :cffi))
(in-package :cl-oclapi-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-oclapi)' in your Lisp.

(plan 5)

(defun set-platform-id (properties platform-id)
  (setf (mem-aref properties 'cl-context-properties 0) +cl-context-platform+)
  (setf (mem-aref properties 'cl-platform-id 1) platform-id)
  (setf (mem-aref properties 'cl-context-properties 2) 0))

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
      (cl-get-device-ids platform +cl-device-type-cpu+ 1 devices num-devices)
      (let ((device (mem-aref devices 'cl-device-id)))
        (subtest "clGetDeviceInfo"
          (is +cl-invalid-device+ (cl-get-device-info (null-pointer)
                                                      0
                                                      0
                                                      (null-pointer)
                                                      (null-pointer)))
          (with-foreign-objects ((param-value 'cl-uchar 256)
                                 (param-value-size-ret 'size-t))
            (is +cl-success+ (cl-get-device-info device
                                                 +cl-device-version+
                                                 256
                                                 param-value
                                                 param-value-size-ret))
            (is "OpenCL 1.2 pocl" (foreign-string-to-lisp param-value))))
        (subtest "clCreateSubDevices"
          (is +cl-invalid-device+ (cl-create-sub-devices (null-pointer)
                                                         (null-pointer)
                                                         0
                                                         (null-pointer)
                                                         (null-pointer))))
        (subtest "clRetainDevice"
          (is +cl-invalid-device+ (cl-retain-device (null-pointer))))
        (subtest "clReleaseDevice"
          (is +cl-invalid-device+ (cl-release-device (null-pointer))))))))

(subtest "Context API"
  (subtest "can call context api functions."
    (ok (cl-create-context (null-pointer)
                           0
                           (null-pointer)
                           (null-pointer)
                           (null-pointer)
                           (null-pointer)))
    (ok (cl-create-context-from-type (null-pointer)
                                     0
                                     (null-pointer)
                                     (null-pointer)
                                     (null-pointer)))
    (is +cl-invalid-context+ (cl-retain-context (null-pointer)))
    (is +cl-invalid-context+ (cl-release-context (null-pointer)))
    (is +cl-invalid-context+ (cl-get-context-info (null-pointer)
                                                  0
                                                  0
                                                  (null-pointer)
                                                  (null-pointer))))
  (with-foreign-objects ((platforms 'cl-platform-id)
                         (num-platforms 'cl-uint)
                         (devices 'cl-device-id)
                         (num-devices 'cl-uint)
                         (properties 'cl-context-properties 3)
                         (errcode-ret 'cl-int))
    (is +cl-success+ (cl-get-platform-ids 1 platforms num-platforms))
    (let ((platform (mem-aref platforms 'cl-platform-id)))
      (is +cl-success+ (cl-get-device-ids platform
                                          +cl-device-type-default+
                                          1
                                          devices
                                          num-devices))
      (subtest "clCreateContext"
        (setf (mem-aref errcode-ret 'cl-int) +cl-success+)
        (let ((context (cl-create-context (null-pointer)
                                          1
                                          devices
                                          (null-pointer)
                                          (null-pointer)
                                          errcode-ret)))
          (is +cl-success+ (mem-aref errcode-ret 'cl-int))
          (ok context)))
      (subtest "clCreateContextFromType"
        (setf (mem-aref errcode-ret 'cl-int) +cl-success+)
        (set-platform-id properties platform)
        (let ((context (cl-create-context-from-type properties
                                                    +cl-device-type-default+
                                                    (null-pointer)
                                                    (null-pointer)
                                                    errcode-ret)))
          (is +cl-success+ (mem-aref errcode-ret 'cl-int))
          (ok context)))
      (subtest "clRetainContext"
        (let ((context (cl-create-context-from-type properties
                                                    +cl-device-type-default+
                                                    (null-pointer)
                                                    (null-pointer)
                                                    errcode-ret)))
          (is +cl-success+ (cl-retain-context context))))
      (subtest "clReleaseContext"
        (let ((context (cl-create-context-from-type properties
                                                    +cl-device-type-default+
                                                    (null-pointer)
                                                    (null-pointer)
                                                    errcode-ret)))
          (is +cl-success+ (cl-release-context context))))
      (subtest "clGetContextInfo"
        (let ((context (cl-create-context-from-type properties
                                                    +cl-device-type-default+
                                                    (null-pointer)
                                                    (null-pointer)
                                                    errcode-ret)))
          (with-foreign-objects ((param-value 'cl-int)
                                 (param-value-size-ret 'cl-int))
            (is +cl-success+ (cl-get-context-info context
                                                  +cl-context-num-devices+
                                                  4
                                                  param-value
                                                  param-value-size-ret))
            (ok (> (mem-aref param-value 'cl-int) 0))
            (ok (> (mem-aref param-value-size-ret 'cl-int) 0)))
          (cl-release-context context))))))

(subtest "Command Queue API"
  (subtest "can call functions."
    (ok (cl-create-command-queue (null-pointer)
                                 (null-pointer)
                                 0
                                 (null-pointer)))
    (is +cl-invalid-command-queue+ (cl-retain-command-queue (null-pointer)))
    (is +cl-invalid-command-queue+ (cl-release-command-queue (null-pointer)))
    (is +cl-invalid-command-queue+ (cl-get-command-queue-info (null-pointer)
                                                              0
                                                              0
                                                              (null-pointer)
                                                              (null-pointer))))
  (subtest "valid params."
    (with-foreign-objects ((platforms 'cl-platform-id)
                           (num-platforms 'cl-uint)
                           (devices 'cl-device-id)
                           (num-devices 'cl-uint)
                           (properties 'cl-context-properties 3)
                           (errcode-ret 'cl-int))
      (is +cl-success+ (cl-get-platform-ids 1 platforms num-platforms))
      (let ((platform (mem-aref platforms 'cl-platform-id)))
        (is +cl-success+ (cl-get-device-ids platform
                                            +cl-device-type-default+
                                            1
                                            devices
                                            num-devices))
        (let ((device (mem-aref devices 'cl-device-id)))
          (set-platform-id properties platform)
          (let ((context (cl-create-context-from-type properties
                                                      +cl-device-type-default+
                                                      (null-pointer)
                                                      (null-pointer)
                                                      errcode-ret)))
            (is +cl-success+ (mem-aref errcode-ret 'cl-int))
            (let ((command-queue (cl-create-command-queue context
                                                          device
                                                          0
                                                          errcode-ret)))
              (is +cl-success+ (mem-aref errcode-ret 'cl-int))
              (with-foreign-objects ((param-value 'cl-uchar 256)
                                     (param-value-size-ret 'size-t))
                (is +cl-success+ (cl-get-command-queue-info command-queue
                                                            +cl-queue-device+
                                                            256
                                                            param-value
                                                            param-value-size-ret))
                (ok (> (mem-aref param-value-size-ret 'size-t) 0))
                (is +cl-success+ (cl-retain-command-queue command-queue))
                (is +cl-success+ (cl-release-command-queue command-queue))))))))))

(subtest "Memmory Object API"
  (subtest "can call functions."
    (ok (cl-create-buffer (null-pointer)
                          0
                          0
                          (null-pointer)
                          (null-pointer)))
    (ok (cl-create-sub-buffer (null-pointer)
                              0
                              0
                              (null-pointer)
                              (null-pointer)))
    (ok (cl-create-image (null-pointer)
                         0
                         (null-pointer)
                         (null-pointer)
                         (null-pointer)
                         (null-pointer)))
    (is +cl-invalid-mem-object+ (cl-retain-mem-object (null-pointer)))
    (is +cl-invalid-mem-object+ (cl-release-mem-object (null-pointer)))
    (is +cl-invalid-context+ (cl-get-supported-image-formats (null-pointer)
                                                             0
                                                             0
                                                             0
                                                             (null-pointer)
                                                             (null-pointer)))
    (is +cl-invalid-mem-object+ (cl-get-mem-object-info (null-pointer)
                                                        0
                                                        0
                                                        (null-pointer)
                                                        (null-pointer)))
    (is +cl-invalid-mem-object+ (cl-get-image-info (null-pointer)
                                                   0
                                                   0
                                                   (null-pointer)
                                                   (null-pointer))))
  (subtest "valid params."
    (with-foreign-objects ((platforms 'cl-platform-id)
                           (num-platforms 'cl-uint)
                           (devices 'cl-device-id)
                           (num-devices 'cl-uint)
                           (properties 'cl-context-properties 3)
                           (errcode-ret 'cl-int))
      (is +cl-success+ (cl-get-platform-ids 1 platforms num-platforms))
      (let ((platform (mem-aref platforms 'cl-platform-id)))
        (is +cl-success+ (cl-get-device-ids platform
                                            +cl-device-type-default+
                                            1
                                            devices
                                            num-devices))
        (set-platform-id properties platform)
        (let ((context (cl-create-context-from-type properties
                                                    +cl-device-type-default+
                                                    (null-pointer)
                                                    (null-pointer)
                                                    errcode-ret)))
          (is +cl-success+ (mem-aref errcode-ret 'cl-int))
          (subtest "buffer"
            (let ((buffer (cl-create-buffer context
                                            +cl-mem-read-write+
                                            1
                                            (null-pointer)
                                            errcode-ret)))
              (is +cl-success+ (mem-aref errcode-ret 'cl-int))
              (is +cl-success+ (cl-retain-mem-object buffer))
              (is +cl-success+ (cl-release-mem-object buffer))))
          (subtest "image"
            (with-foreign-objects ((format '(:struct cl-image-format))
                                   (desc '(:struct cl-image-desc)))
              (setf (foreign-slot-value format '(:struct cl-image-format) 'image-channel-order)
                    +cl-rgba+
                    (foreign-slot-value format '(:struct cl-image-format) 'image-channel-data-type)
                    +cl-unsigned-int8+)

              (setf (foreign-slot-value desc '(:struct cl-image-desc) 'image-type) +cl-mem-object-image2d+
                    (foreign-slot-value desc '(:struct cl-image-desc) 'image-width) 1
                    (foreign-slot-value desc '(:struct cl-image-desc) 'image-height) 1
                    (foreign-slot-value desc '(:struct cl-image-desc) 'image-depth) 1
                    (foreign-slot-value desc '(:struct cl-image-desc) 'image-array-size) 1
                    (foreign-slot-value desc '(:struct cl-image-desc) 'image-row-pitch) 1
                    (foreign-slot-value desc '(:struct cl-image-desc) 'image-slice-pitch) 1
                    (foreign-slot-value desc '(:struct cl-image-desc) 'num-mip-levels) 0
                    (foreign-slot-value desc '(:struct cl-image-desc) 'num-samples) 0
                    (foreign-slot-value desc '(:struct cl-image-desc) 'buffer) (null-pointer))
              (let ((image (cl-create-image context
                                            +cl-mem-read-write+
                                            format
                                            desc
                                            (null-pointer)
                                            errcode-ret)))
                (is +cl-success+ (mem-aref errcode-ret 'cl-int))
                (is +cl-success+ (cl-retain-mem-object image))
                (is +cl-success+ (cl-release-mem-object image))))))))))

(finalize)
