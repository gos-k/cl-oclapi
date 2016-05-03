(in-package :cl-user)
(defpackage cl-oclapi-test.copy
  (:use :cl
        :cl-oclapi
        :prove
        :cffi
        :cl-oclapi-test.init))
(in-package :cl-oclapi-test.copy)

(plan nil)

(defparameter *copy-kernel*
  "__kernel void alfa(__global char * a) { a[0] = 1; }")

(subtest "Copy buffer"
  (with-foreign-objects ((devices 'cl-device-id)
                         (num-devices 'cl-uint)
                         (errcode-ret 'cl-int))
    (let ((platform (get-platform-id)))
      (ok platform "get platform")
      (is-success (cl-get-device-ids platform
                                     +cl-device-type-default+
                                     1
                                     devices
                                     num-devices) "get device")
      (let ((context (create-context (null-pointer) 1 devices))
            (device (mem-aref devices 'cl-device-id)))
        (ok context "create context")
        (let ((in (create-buffer context +cl-mem-read-only+ 1)))
          (ok in "create buffer")
          (let ((out (create-buffer context +cl-mem-write-only+ 1)))
            (ok out "create buffer")
            (let ((command-queue (cl-create-command-queue context
                                                          device
                                                          0
                                                          errcode-ret)))
              (is-success (mem-aref errcode-ret 'cl-int) "create command queue")
              (ok command-queue "create command queue")
              (with-foreign-objects ((src-offset 'size-t)
                                     (dst-offset 'size-t)
                                     (size 'size-t))
                (setf (mem-aref src-offset 'size-t) 0
                      (mem-aref dst-offset 'size-t) 0
                      (mem-aref size 'size-t) 1)
                (is-success (cl-enqueue-copy-buffer command-queue
                                                    in
                                                    out
                                                    (mem-aref src-offset 'size-t)
                                                    (mem-aref dst-offset 'size-t)
                                                    (mem-aref size 'size-t)
                                                    0
                                                    (null-pointer)
                                                    (null-pointer)) "enqueue copy buffer"))
              (is-success (cl-finish command-queue) "finish"))
            (is-success (cl-release-mem-object out) "release mem object"))
          (is-success (cl-release-mem-object in) "release mem object"))
        (is-success (cl-release-context context) "release context")
        (is-success (cl-release-device device) "release device")))))

(subtest "Copy kernel"
  (with-foreign-objects ((devices 'cl-device-id)
                         (num-devices 'cl-uint)
                         (errcode-ret 'cl-int))
    (let ((platform (get-platform-id)))
      (ok platform "get platform")
      (is-success (cl-get-device-ids platform
                                     +cl-device-type-default+
                                     1
                                     devices
                                     num-devices) "get device")
      (let ((context (create-context (null-pointer) 1 devices))
            (device (mem-aref devices 'cl-device-id)))
        (ok context "create context")
        (let ((buffer (create-buffer context +cl-mem-read-write+ 1000)))
          (ok buffer "create buffer")
          (with-foreign-string (source *copy-kernel*)
            (with-foreign-object (p :pointer)
              (setf (mem-aref p :pointer) source)
              (let ((program (create-program-with-source context 1 p)))
                (ok program "create program")
                (is-success (cl-build-program program
                                              1
                                              devices
                                              (null-pointer)
                                              (null-pointer)
                                              (null-pointer)) "build program")
                (with-foreign-string (name "alfa")
                  (let ((kernel (create-kernel program name)))
                    (ok kernel "create kernel")
                    (with-foreign-object (p :pointer)
                      (setf (mem-aref p :pointer) buffer)
                      (is-success (cl-set-kernel-arg kernel 0 8 p) "set kernel arg")
                      (let ((command-queue (cl-create-command-queue context
                                                                    device
                                                                    0
                                                                    errcode-ret)))
                        (is-success (mem-aref errcode-ret 'cl-int) "create command queue")
                        (ok command-queue "create command queue")
                        (with-foreign-objects ((global-work-size 'size-t 3)
                                               (local-work-size 'size-t 3))
                          (setf (mem-aref global-work-size 'size-t 0) 1
                                (mem-aref global-work-size 'size-t 1) 0
                                (mem-aref global-work-size 'size-t 2) 0)
                          (setf (mem-aref local-work-size 'size-t 0) 1
                                (mem-aref local-work-size 'size-t 1) 0
                                (mem-aref local-work-size 'size-t 2) 0)
                          (is-success (cl-enqueue-ndrange-kernel command-queue
                                                                 kernel
                                                                 1
                                                                 (null-pointer)
                                                                 global-work-size
                                                                 local-work-size
                                                                 0
                                                                 (null-pointer)
                                                                 (null-pointer)) "enqueue kernel"))
                        (is-success (cl-finish command-queue) "finish")
                        (with-foreign-object (value 'cl-char)
                          (setf (mem-aref value 'cl-char) 0)
                          (is-success (cl-enqueue-read-buffer command-queue
                                                              buffer
                                                              0
                                                              0
                                                              1
                                                              value
                                                              0
                                                              (null-pointer)
                                                              (null-pointer)) "enqueue read buffer")

                          (is-success (cl-finish command-queue) "finish")
                          (is 1 (mem-aref value 'cl-char) "result"))
                        (is-success (cl-release-command-queue command-queue) "release command queueu")))
                    (is-success (cl-release-kernel kernel) "release kernel")))
                (is-success (cl-release-program program) "release program"))))
          (is-success (cl-release-mem-object buffer) "release mem object"))
        (is-success (cl-release-context context) "release context")
        (is-success (cl-release-device device) "release device")))))

(finalize)
