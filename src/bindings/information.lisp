#|
  This file is a part of cl-oclapi project.
  Copyright (c) 2016 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-oclapi.information
  (:use :cl
        :cffi
        :cl-annot
        :cl-oclapi.constants
        :cl-oclapi.types))
(in-package :cl-oclapi.information)

(syntax:use-syntax :annot)

#| cl-device-info types |#
@export
(defparameter *cl-device-info-types*
  `((,+cl-device-type+ . cl-device-type)
    (,+cl-device-vendor-id+ . cl-uint)
    (,+cl-device-max-compute-units+ . cl-uint)
    (,+cl-device-max-work-item-dimensions+ . cl-uint)
    (,+cl-device-max-work-group-size+ . cl-size-t)
    ;;+cl-device-max-work-item-sizes+ vector-class< cl-size-t>
    (,+cl-device-preferred-vector-width-char+ . cl-uint)
    (,+cl-device-preferred-vector-width-short+ . cl-uint)
    (,+cl-device-preferred-vector-width-int+ . cl-uint)
    (,+cl-device-preferred-vector-width-long+ . cl-uint)
    (,+cl-device-preferred-vector-width-float+ . cl-uint)
    (,+cl-device-preferred-vector-width-double+ . cl-uint)
    (,+cl-device-max-clock-frequency+ . cl-uint)
    (,+cl-device-address-bits+ . cl-uint)
    (,+cl-device-max-read-image-args+ . cl-uint)
    (,+cl-device-max-write-image-args+ . cl-uint)
    (,+cl-device-max-mem-alloc-size+ . cl-ulong)
    (,+cl-device-image2d-max-width+ . cl-size-t)
    (,+cl-device-image2d-max-height+ . cl-size-t)
    (,+cl-device-image3d-max-width+ . cl-size-t)
    (,+cl-device-image3d-max-height+ . cl-size-t)
    (,+cl-device-image3d-max-depth+ . cl-size-t)
    (,+cl-device-image-support+ . cl-bool)
    (,+cl-device-max-parameter-size+ . cl-size-t)
    (,+cl-device-max-samplers+ . cl-uint)
    (,+cl-device-mem-base-addr-align+ . cl-uint)
    (,+cl-device-min-data-type-align-size+ . cl-uint)
    (,+cl-device-single-fp-config+ . cl-device-fp-config)
    (,+cl-device-global-mem-cache-type+ . cl-device-mem-cache-type)
    (,+cl-device-global-mem-cacheline-size+ . cl-uin)
    (,+cl-device-global-mem-cache-size+ . cl-ulong)
    (,+cl-device-global-mem-size+ . cl-ulong)
    (,+cl-device-max-constant-buffer-size+ . cl-ulong)
    (,+cl-device-max-constant-args+ . cl-uint)
    (,+cl-device-local-mem-type+ . cl-device-local-mem-type)
    (,+cl-device-local-mem-size+ . cl-ulong)
    (,+cl-device-error-correction-support+ . cl-bool)
    (,+cl-device-profiling-timer-resolution+ . cl-size-t)
    (,+cl-device-endian-little+ . cl-bool)
    (,+cl-device-available+ . cl-bool)
    (,+cl-device-compiler-available+ . cl-bool)
    (,+cl-device-execution-capabilities+ . cl-device-exec-capabilities)
    (,+cl-device-queue-properties+ . cl-command-queue-properties)
    (,+cl-device-platform+ . cl-platform-id)
    (,+cl-device-name+ . string)
    (,+cl-device-vendor+ . string)
    (,+cl-driver-version+ . string)
    (,+cl-device-profile+ . string)
    (,+cl-device-version+ . string)
    (,+cl-device-extensions+ . string)
    (,+cl-device-parent-device+ . cl-device-id)
    ;;+cl-device-partition-properties+ vector-class<cl-device-partition-property>
    ;;+cl-device-partition-type+ vector-class<cl-device-partition-property>
    (,+cl-device-reference-count+ . cl-uint)
    (,+cl-device-preferred-interop-user-sync+ . cl-size-t)
    (,+cl-device-partition-affinity-domain+ . cl-device-affinity-domain)
    (,+cl-device-built-in-kernels+ . string)))
