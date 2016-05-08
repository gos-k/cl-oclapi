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
  "__kernel void alfa(__global char * a, __global char * b) { a[0] = b[0]; }")

(subtest "Copy buffer"
  (with-foreign-objects ((devices 'cl-device-id)
                         (num-devices 'cl-uint))
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
            (let ((command-queue (create-command-queue context device 0)))
              (ok command-queue "create command queue")
              (with-foreign-objects ((src-offset 'size-t)
                                     (dst-offset 'size-t)
                                     (size 'size-t))
                (setf (mem-aref src-offset 'size-t) 0
                      (mem-aref dst-offset 'size-t) 0
                      (mem-aref size 'size-t) 1)
                (enqueue-copy-buffer command-queue
                                     in
                                     out
                                     (mem-aref src-offset 'size-t)
                                     (mem-aref dst-offset 'size-t)
                                     (mem-aref size 'size-t)))
              (is-success (cl-finish command-queue) "finish"))
            (is-success (cl-release-mem-object out) "release mem object"))
          (is-success (cl-release-mem-object in) "release mem object"))
        (is-success (cl-release-context context) "release context")
        (is-success (cl-release-device device) "release device")))))

(subtest "Copy kernel"
  (with-foreign-objects ((devices 'cl-device-id)
                         (num-devices 'cl-uint))
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
        (let ((buffer-in (create-buffer context +cl-mem-read-only+ 1))
              (buffer-out (create-buffer context +cl-mem-write-only+ 1)))
          (ok buffer-in "create buffer-in")
          (ok buffer-out "create buffer-out")
          (with-foreign-string (source *copy-kernel*)
            (with-foreign-object (p :pointer)
              (setf (mem-aref p :pointer) source)
              (let ((program (create-program-with-source context 1 p)))
                (ok program "create program")
                (build-program program
                               1
                               devices)
                (with-foreign-string (name "alfa")
                  (let ((kernel (create-kernel program name)))
                    (ok kernel "create kernel")
                    (with-foreign-objects ((p-in :pointer)
                                           (p-out :pointer))
                      (setf (mem-aref p-in :pointer) buffer-in)
                      (setf (mem-aref p-out :pointer) buffer-out)
                      (is-success (cl-set-kernel-arg kernel 0 8 p-out) "set kernel arg")
                      (is-success (cl-set-kernel-arg kernel 1 8 p-in) "set kernel arg")
                      (let ((command-queue (create-command-queue context device 0)))
                        (ok command-queue "create command queue")
                        (with-foreign-object (value 'cl-char)
                          (setf (mem-aref value 'cl-char) 1)
                          (enqueue-write-buffer command-queue buffer-in 1 0 1 value)
                          (setf (mem-aref value 'cl-char) 2)
                          (enqueue-write-buffer command-queue buffer-out 1 0 1 value))
                        (with-foreign-objects ((global-work-size 'size-t 3)
                                               (local-work-size 'size-t 3))
                          (set-work-size global-work-size 1)
                          (set-work-size local-work-size 1)
                          (enqueue-ndrange-kernel command-queue
                                                  kernel
                                                  1
                                                  (null-pointer)
                                                  global-work-size
                                                  local-work-size))
                        (is-success (cl-finish command-queue) "finish")
                        (with-foreign-object (value 'cl-char)
                          (setf (mem-aref value 'cl-char) 0)
                          (enqueue-read-buffer command-queue buffer-out 1 0 1 value)
                          (is 1 (mem-aref value 'cl-char) "buffer-out result"))
                        (is-success (cl-release-command-queue command-queue) "release command queueu")))
                    (is-success (cl-release-kernel kernel) "release kernel")))
                (is-success (cl-release-program program) "release program"))))
          (is-success (cl-release-mem-object buffer-in) "release mem object")
          (is-success (cl-release-mem-object buffer-out) "release mem object"))
        (is-success (cl-release-context context) "release context")
        (is-success (cl-release-device device) "release device")))))

(finalize)
