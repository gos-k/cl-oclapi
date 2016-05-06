(in-package :cl-user)
(defpackage cl-oclapi-test
  (:use :cl
   :cl-oclapi
        :prove
   :cffi
        :cl-oclapi-test.init))
(in-package :cl-oclapi-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-oclapi)' in your Lisp.

(plan nil)

(subtest "platform API"
  (with-foreign-objects ((platforms 'cl-platform-id)
                         (num-platforms 'cl-uint))
    (subtest "clGetPlatformIDs"
      (is +cl-invalid-value+ (cl-get-platform-ids 0
                                                  (null-pointer)
                                                  (null-pointer)))
      (is-success (cl-get-platform-ids 1 platforms num-platforms))
      (ok (> (mem-aref num-platforms 'cl-uint) 0)))
    (subtest "clGetPlatformInfo"
      (is +cl-invalid-value+ (cl-get-platform-info (null-pointer)
                                                   0
                                                   0
                                                   (null-pointer)
                                                   (null-pointer)))
      (cl-get-platform-ids 1 platforms num-platforms)
      (let ((platform (mem-aref platforms 'cl-platform-id)))
        (with-foreign-objects ((param-value 'cl-uchar 1024)
                               (param-value-size-ret 'size-t))
          (is-success (cl-get-platform-info platform
                                            +cl-platform-profile+
                                            1024
                                            param-value
                                            param-value-size-ret))
          (is "FULL_PROFILE" (foreign-string-to-lisp param-value))
          (is-success (cl-get-platform-info platform
                                            +cl-platform-version+
                                            1024
                                            param-value
                                            param-value-size-ret))
          (ok (search "OpenCL" (foreign-string-to-lisp param-value)))
          (is-success (cl-get-platform-info platform
                                            +cl-platform-name+
                                            1024
                                            param-value
                                            param-value-size-ret))
          (let ((platform-name (foreign-string-to-lisp param-value)))
            (ok (or (string= "Portable Computing Language" platform-name)
                    (string= "NVIDIA CUDA" platform-name))))
          (is-success (cl-get-platform-info platform
                                            +cl-platform-vendor+
                                            1024
                                            param-value
                                            param-value-size-ret))
          (let ((platform-vendor (foreign-string-to-lisp param-value)))
            (ok (or (string= "The pocl project" platform-vendor)
                    (string= "NVIDIA Corporation" platform-vendor))))
          (is-success (cl-get-platform-info platform
                                            +cl-platform-extensions+
                                            1024
                                            param-value
                                            param-value-size-ret))
          (ok (search "cl_khr_icd" (foreign-string-to-lisp param-value))))))))

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
        (is-success (cl-get-device-ids platform
                                       +cl-device-type-cpu+
                                       1
                                       devices
                                       num-devices))
        (ok (> (mem-aref num-devices 'cl-uint) 0)))
      (is-success (cl-get-device-ids platform +cl-device-type-default+ 1 devices num-devices))
      (let ((device (mem-aref devices 'cl-device-id)))
        (subtest "clGetDeviceInfo"
          (is +cl-invalid-device+ (cl-get-device-info (null-pointer)
                                                      0
                                                      0
                                                      (null-pointer)
                                                      (null-pointer)))
          (with-foreign-objects ((param-value 'cl-uchar 256)
                                 (param-value-size-ret 'size-t))
            (is-success (cl-get-device-info device
                                            +cl-device-version+
                                            256
                                            param-value
                                            param-value-size-ret))
            (ok (search "OpenCL" (foreign-string-to-lisp param-value)))))
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
    (is-null (cl-create-context (null-pointer)
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
    (is-success (cl-get-platform-ids 1 platforms num-platforms))
    (let ((platform (mem-aref platforms 'cl-platform-id)))
      (is-success (cl-get-device-ids platform
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
          (is-success (mem-aref errcode-ret 'cl-int))
          (ok context)))
      (subtest "clCreateContextFromType"
        (setf (mem-aref errcode-ret 'cl-int) +cl-success+)
        (set-platform-id properties platform)
        (let ((context (cl-create-context-from-type properties
                                                    +cl-device-type-default+
                                                    (null-pointer)
                                                    (null-pointer)
                                                    errcode-ret)))
          (is-success (mem-aref errcode-ret 'cl-int))
          (ok context)))
      (subtest "clRetainContext"
        (let ((context (cl-create-context-from-type properties
                                                    +cl-device-type-default+
                                                    (null-pointer)
                                                    (null-pointer)
                                                    errcode-ret)))
          (is-success (cl-retain-context context))))
      (subtest "clReleaseContext"
        (let ((context (cl-create-context-from-type properties
                                                    +cl-device-type-default+
                                                    (null-pointer)
                                                    (null-pointer)
                                                    errcode-ret)))
          (is-success (cl-release-context context))))
      (subtest "clGetContextInfo"
        (let ((context (cl-create-context-from-type properties
                                                    +cl-device-type-default+
                                                    (null-pointer)
                                                    (null-pointer)
                                                    errcode-ret)))
          (with-foreign-objects ((param-value 'cl-int)
                                 (param-value-size-ret 'cl-int))
            (is-success (cl-get-context-info context
                                             +cl-context-num-devices+
                                             4
                                             param-value
                                             param-value-size-ret))
            (ok (> (mem-aref param-value 'cl-int) 0))
            (ok (> (mem-aref param-value-size-ret 'cl-int) 0)))
          (cl-release-context context))))))

(subtest "Command Queue API"
  (subtest "can call functions."
    (is-null (cl-create-command-queue (null-pointer)
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
      (is-success (cl-get-platform-ids 1 platforms num-platforms))
      (let ((platform (mem-aref platforms 'cl-platform-id)))
        (is-success (cl-get-device-ids platform
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
            (is-success (mem-aref errcode-ret 'cl-int))
            (let ((command-queue (cl-create-command-queue context
                                                          device
                                                          0
                                                          errcode-ret)))
              (is-success (mem-aref errcode-ret 'cl-int))
              (with-foreign-objects ((param-value 'cl-uchar 256)
                                     (param-value-size-ret 'size-t))
                (is-success (cl-get-command-queue-info command-queue
                                                       +cl-queue-device+
                                                       256
                                                       param-value
                                                       param-value-size-ret))
                (ok (> (mem-aref param-value-size-ret 'size-t) 0))
                (is-success (cl-retain-command-queue command-queue))
                (is-success (cl-release-command-queue command-queue))))))))))

(subtest "Memmory Object API"
  (subtest "can call functions."
    (is-null (cl-create-buffer (null-pointer)
                               0
                               0
                               (null-pointer)
                               (null-pointer)))
    (is-null (cl-create-sub-buffer (null-pointer)
                                   0
                                   0
                                   (null-pointer)
                                   (null-pointer)))
    (is-null (cl-create-image (null-pointer)
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
      (is-success (cl-get-platform-ids 1 platforms num-platforms))
      (let ((platform (mem-aref platforms 'cl-platform-id)))
        (is-success (cl-get-device-ids platform
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
          (is-success (mem-aref errcode-ret 'cl-int))
          (subtest "buffer"
            (let ((buffer (cl-create-buffer context
                                            +cl-mem-read-write+
                                            1
                                            (null-pointer)
                                            errcode-ret)))
              (is-success (mem-aref errcode-ret 'cl-int))
              (is-success (cl-retain-mem-object buffer))
              (is-success (cl-release-mem-object buffer))))
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
                (is-success (mem-aref errcode-ret 'cl-int))
                (is-success (cl-retain-mem-object image))
                (is-success (cl-release-mem-object image))))))))))

(subtest "Program API"
  (subtest "can call functions."
    (is-null (cl-create-program-with-source (null-pointer)
                                            0
                                            (null-pointer)
                                            (null-pointer)
                                            (null-pointer)))
    (is-null (cl-create-program-with-binary (null-pointer)
                                            0
                                            (null-pointer)
                                            (null-pointer)
                                            (null-pointer)
                                            (null-pointer)
                                            (null-pointer)))
    (is-null (cl-create-program-with-built-in-kernels (null-pointer)
                                                      0
                                                      (null-pointer)
                                                      (null-pointer)
                                                      (null-pointer)))
    (is +cl-invalid-program+ (cl-retain-program (null-pointer)))
    (is +cl-invalid-program+ (cl-release-program (null-pointer)))
    (is +cl-invalid-program+ (cl-build-program (null-pointer)
                                               0
                                               (null-pointer)
                                               (null-pointer)
                                               (null-pointer)
                                               (null-pointer)))
    (is +cl-invalid-program+ (cl-compile-program (null-pointer)
                                                 0
                                                 (null-pointer)
                                                 (null-pointer)
                                                 0
                                                 (null-pointer)
                                                 (null-pointer)
                                                 (null-pointer)
                                                 (null-pointer)))
    (is-null (cl-link-program (null-pointer)
                              0
                              (null-pointer)
                              (null-pointer)
                              0
                              (null-pointer)
                              (null-pointer)
                              (null-pointer)))
                                        ;(is +cl-invalid-platform+ (cl-unload-platform-compiler (null-pointer)))
    (is +cl-invalid-program+ (cl-get-program-info (null-pointer)
                                                  0
                                                  0
                                                  (null-pointer)
                                                  (null-pointer)))
    (is +cl-invalid-program+ (cl-get-program-build-info (null-pointer)
                                                        (null-pointer)
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
      (is-success (cl-get-platform-ids 1 platforms num-platforms))
      (let ((platform (mem-aref platforms 'cl-platform-id)))
        (is-success (cl-get-device-ids platform
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
          (is-success (mem-aref errcode-ret 'cl-int))
          (with-foreign-string (s "")
            (with-foreign-objects ((p :pointer)
                                   (length 'size-t))
              (setf (mem-aref p :pointer) s)
              (setf (mem-aref length 'size-t) 0)
              (let ((program (cl-create-program-with-source context
                                                            1
                                                            p
                                                            length
                                                            errcode-ret)))
                (ok program)
                (is-success (mem-aref errcode-ret 'cl-int))
                (is-success (cl-retain-program program))
                (is-success (cl-release-program program))))))))))

(subtest "Kernel Object API"
  (subtest "can call functions."
    (is-null (cl-create-kernel (null-pointer)
                               (null-pointer)
                               (null-pointer)))
    (is +cl-invalid-program+ (cl-create-kernels-in-program (null-pointer)
                                                           0
                                                           (null-pointer)
                                                           (null-pointer)))
    (is +cl-invalid-kernel+ (cl-retain-kernel (null-pointer)))
    (is +cl-invalid-kernel+ (cl-release-kernel (null-pointer)))
    (is +cl-invalid-kernel+ (cl-set-kernel-arg (null-pointer)
                                               0
                                               0
                                               (null-pointer)))
    (is +cl-invalid-kernel+ (cl-get-kernel-info (null-pointer)
                                                0
                                                0
                                                (null-pointer)
                                                (null-pointer)))
    (is +cl-invalid-kernel+ (cl-get-kernel-arg-info (null-pointer)
                                                    0
                                                    0
                                                    0
                                                    (null-pointer)
                                                    (null-pointer)))
    (is +cl-invalid-kernel+ (cl-get-kernel-work-group-info (null-pointer)
                                                           (null-pointer)
                                                           0
                                                           0
                                                           (null-pointer)
                                                           (null-pointer)))))

(subtest "Event Object APIs"
  (subtest "can call functions."
    (is +cl-invalid-value+ (cl-wait-for-events 0 (null-pointer)))
    (is +cl-invalid-event+ (cl-get-event-info (null-pointer)
                                              0
                                              0
                                              (null-pointer)
                                              (null-pointer)))
    (is-null (cl-create-user-event (null-pointer)
                                   (null-pointer)))
    (is +cl-invalid-event+ (cl-retain-event (null-pointer)))
    (is +cl-invalid-event+ (cl-release-event (null-pointer)))
    (is +cl-invalid-event+ (cl-set-user-event-status (null-pointer)
                                                     0))))

(subtest "Profiling APIs"
  (is +cl-invalid-event+ (cl-get-event-profiling-info (null-pointer)
                                                      0
                                                      0
                                                      (null-pointer)
                                                      (null-pointer))))

(subtest "Flush and Finish API"
  (subtest "can call functions."
    (is +cl-invalid-command-queue+ (cl-flush (null-pointer)))
    (is +cl-invalid-command-queue+ (cl-finish (null-pointer)))))

(subtest "Enqueued Commands APIs"
  (subtest "can call functions."
    (is +cl-invalid-command-queue+
        (cl-enqueue-read-buffer (null-pointer)
                                (null-pointer)
                                0
                                0
                                0
                                (null-pointer)
                                0
                                (null-pointer)
                                (null-pointer)))
    (is +cl-invalid-command-queue+
        (cl-enqueue-read-buffer-rect (null-pointer)
                                     (null-pointer)
                                     0
                                     (null-pointer)
                                     (null-pointer)
                                     (null-pointer)
                                     0
                                     0
                                     0
                                     0
                                     (null-pointer)
                                     0
                                     (null-pointer)
                                     (null-pointer)))
    (is +cl-invalid-command-queue+
        (cl-enqueue-write-buffer (null-pointer)
                                 (null-pointer)
                                 0
                                 0
                                 0
                                 (null-pointer)
                                 0
                                 (null-pointer)
                                 (null-pointer)))
    (is +cl-invalid-command-queue+
        (cl-enqueue-write-buffer-rect (null-pointer)
                                      (null-pointer)
                                      0
                                      (null-pointer)
                                      (null-pointer)
                                      (null-pointer)
                                      0
                                      0
                                      0
                                      0
                                      (null-pointer)
                                      0
                                      (null-pointer)
                                      (null-pointer)))
    (is +cl-invalid-command-queue+
        (cl-enqueue-fill-buffer (null-pointer)
                                (null-pointer)
                                (null-pointer)
                                0
                                0
                                0
                                0
                                (null-pointer)
                                (null-pointer)))
    (is +cl-invalid-command-queue+
        (cl-enqueue-copy-buffer (null-pointer)
                                (null-pointer)
                                (null-pointer)
                                0
                                0
                                0
                                0
                                (null-pointer)
                                (null-pointer)))
    (is +cl-invalid-command-queue+
        (cl-enqueue-copy-buffer-rect (null-pointer)
                                     (null-pointer)
                                     (null-pointer)
                                     (null-pointer)
                                     (null-pointer)
                                     (null-pointer)
                                     0
                                     0
                                     0
                                     0
                                     0
                                     (null-pointer)
                                     (null-pointer)))
    (is +cl-invalid-command-queue+
        (cl-enqueue-read-image (null-pointer)
                               (null-pointer)
                               0
                               (null-pointer)
                               (null-pointer)
                               0
                               0
                               (null-pointer)
                               0
                               (null-pointer)
                               (null-pointer)))
    (is +cl-invalid-command-queue+
        (cl-enqueue-write-image (null-pointer)
                                (null-pointer)
                                0
                                (null-pointer)
                                (null-pointer)
                                0
                                0
                                (null-pointer)
                                0
                                (null-pointer)
                                (null-pointer)))
    (is +cl-invalid-command-queue+
        (cl-enqueue-fill-image (null-pointer)
                               (null-pointer)
                               (null-pointer)
                               (null-pointer)
                               (null-pointer)
                               0
                               (null-pointer)
                               (null-pointer)))
    (is +cl-invalid-command-queue+
        (cl-enqueue-copy-image (null-pointer)
                               (null-pointer)
                               (null-pointer)
                               (null-pointer)
                               (null-pointer)
                               (null-pointer)
                               0
                               (null-pointer)
                               (null-pointer)))
    (is +cl-invalid-command-queue+
        (cl-enqueue-copy-image-to-buffer (null-pointer)
                                         (null-pointer)
                                         (null-pointer)
                                         (null-pointer)
                                         (null-pointer)
                                         0
                                         0
                                         (null-pointer)
                                         (null-pointer)))
    (is +cl-invalid-command-queue+
        (cl-enqueue-copy-buffer-to-image (null-pointer)
                                         (null-pointer)
                                         (null-pointer)
                                         0
                                         (null-pointer)
                                         (null-pointer)
                                         0
                                         (null-pointer)
                                         (null-pointer)))
    (is-null (cl-enqueue-map-buffer (null-pointer)
                                    (null-pointer)
                                    0
                                    0
                                    0
                                    0
                                    0
                                    (null-pointer)
                                    (null-pointer)
                                    (null-pointer)))
    (is-null (cl-enqueue-map-image (null-pointer)
                                   (null-pointer)
                                   0
                                   0
                                   (null-pointer)
                                   (null-pointer)
                                   (null-pointer)
                                   (null-pointer)
                                   0
                                   (null-pointer)
                                   (null-pointer)
                                   (null-pointer)))
    (is +cl-invalid-command-queue+
        (cl-enqueue-unmap-mem-object (null-pointer)
                                     (null-pointer)
                                     (null-pointer)
                                     0
                                     (null-pointer)
                                     (null-pointer)))
    (is +cl-invalid-command-queue+
        (cl-enqueue-migrate-mem-objects (null-pointer)
                                        0
                                        (null-pointer)
                                        0
                                        0
                                        (null-pointer)
                                        (null-pointer)))
    (is +cl-invalid-command-queue+
        (cl-enqueue-ndrange-kernel (null-pointer)
                                   (null-pointer)
                                   0
                                   (null-pointer)
                                   (null-pointer)
                                   (null-pointer)
                                   0
                                   (null-pointer)
                                   (null-pointer)))
    (is +cl-invalid-command-queue+
        (cl-enqueue-task (null-pointer)
                         (null-pointer)
                         0
                         (null-pointer)
                         (null-pointer)))
    (is +cl-invalid-command-queue+
        (cl-enqueue-native-kernel (null-pointer)
                                  (null-pointer)
                                  (null-pointer)
                                  0
                                  0
                                  (null-pointer)
                                  (null-pointer)
                                  0
                                  (null-pointer)
                                  (null-pointer)))
    (is +cl-invalid-command-queue+
        (cl-enqueue-markerwith-wait-list (null-pointer)
                                         0
                                         (null-pointer)
                                         (null-pointer)))
    (is +cl-invalid-command-queue+
        (cl-enqueue-barrier-with-wait-list (null-pointer)
                                           0
                                           (null-pointer)
                                           (null-pointer)))))

(finalize)
