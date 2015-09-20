(in-package :cl-user)
(defpackage cl-oclapi
  (:use :cl :cffi)
  (:export :cl-get-platform-ids
           :cl-get-platform-info))
(in-package :cl-oclapi)

(define-foreign-library libopencl
  (t (:default "libOpenCL")))

(use-foreign-library libopencl)

#| cl-platform.h |#
(defctype cl-char :int8)
(defctype cl-uchar :uint8)
(defctype cl-short :int16)
(defctype cl-ushort :uint16)
(defctype cl-int :int32)
(defctype cl-uint :uint32)
(defctype cl-long :int64)
(defctype cl-ulong :uint64)

(defctype cl-half :uint16)
(defctype cl-float :float)
(defctype cl-double :double)

(defctype size-t :uint64)
(defctype intptr-t :int64)
(defctype uintptr-t :uint64)

#| cl.h |#
(defctype cl-platform-id :pointer)
(defctype cl-device-id :pointer)
(defctype cl-context :pointer)
(defctype cl-command-queue :pointer)
(defctype cl-mem :pointer)
(defctype cl-program :pointer)
(defctype cl-kernel :pointer)
(defctype cl-event :pointer)
(defctype cl-sampler :pointer)

(defctype cl-bool cl-uint)
(defctype cl-bitfield cl-ulong)
(defctype cl-device-type cl-bitfield)
(defctype cl-platform-info cl-uint)
(defctype cl-device-info cl-uint)
(defctype cl-device-fp-config cl-bitfield)
(defctype cl-device-mem-cache-type cl-uint)
(defctype cl-device-local-mem-type cl-uint)
(defctype cl-device-exec-capabilities cl-bitfield)
(defctype cl-command-queue-properties cl-bitfield)
(defctype cl-device-partition-property intptr-t)
(defctype cl-device-affinity-domain cl-bitfield)

(defctype cl-context-properties intptr-t)
(defctype cl-context-info cl-uint)
(defctype cl-command-queue-info cl-uint)
(defctype cl-channel-order cl-uint)
(defctype cl-channel-type cl-uint)
(defctype cl-mem-flags cl-bitfield)
(defctype cl-mem-object-type cl-uint)
(defctype cl-mem-info cl-uint)
(defctype cl-mem-migration-flags cl-bitfield)
(defctype cl-image-info cl-uint)
(defctype cl-buffer-create-type cl-uint)
(defctype cl-addressing-mode cl-uint)
(defctype cl-filter-mode cl-uint)
(defctype cl-sampler-info cl-uint)
(defctype cl-map-flags cl-bitfield)
(defctype cl-program-info cl-uint)
(defctype cl-program-build-info cl-uint)
(defctype cl-program-binary-type cl-uint)
(defctype cl-build-status cl-int)
(defctype cl-kernel-info cl-uint)
(defctype cl-kernel-arg-info cl-uint)
(defctype cl-kernel-arg-address-qualifier cl-uint)
(defctype cl-kernel-arg-access-qualifier cl-uint)
(defctype cl-kernel-arg-type-qualifier cl-bitfield)
(defctype cl-kernel-work-group-info cl-uint)
(defctype cl-event-info cl-uint)
(defctype cl-command-type cl-uint)
(defctype cl-profiling-info cl-uint)

(defcstruct cl-image-format
  (image-channel-order cl-channel-order)
  (image-channel-data-type cl-channel-type))

(defcstruct cl-image-desc
  (image-type cl-mem-object-type)
  (image-width size-t)
  (image-height size-t)
  (image-depth size-t)
  (image-array-size size-t)
  (image-row-pitch size-t)
  (image-slice-pitch size-t)
  (num-mip-levels cl-uint)
  (num-samples cl-uint)
  (buffer cl-mem))

(defcstruct cl-buffer-region
  (origin size-t)
  (size size-t))

#| cl.h - platform API. |#

;; CL_API_SUFFIX__VERSION_1_0;
(defcfun ("clGetPlatformIDs" cl-get-platform-ids) cl-int
  (num-entries cl-uint)
  (platforms (:pointer cl-platform-id))
  (num-platforms (:pointer cl-uint)))

;; CL_API_SUFFIX__VERSION_1_0;
(defcfun ("clGetPlatformInfo" cl-get-platform-info) cl-int
  (platform cl-platform-id)
  (param-name cl-platform-info)
  (param-value-size size-t)
  (param-value :pointer)
  (param-value-size-ret (:pointer size-t)))
