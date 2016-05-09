#|
  This file is a part of cl-oclapi project.
  Copyright (c) 2015 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-oclapi.types
  (:use :cl :cffi))
(in-package :cl-oclapi.types)

(defmacro defctypes (&body types)
  `(progn
     ,@(loop for type in types
             collect `(export ',(car type))
             collect `(defctype ,@type))))

(defmacro defcstruct-export (name &body slots)
  `(progn
     (defcstruct ,name
       ,@slots)
     (export ',name)
     ,@(loop for slot in slots
             collect `(export ',(car slot)))))

#| cl-platform.h |#
(defctypes
  (cl-char :int8)
  (cl-uchar :uint8)
  (cl-short :int16)
  (cl-ushort :uint16)
  (cl-int :int32)
  (cl-uint :uint32)
  (cl-long :int64)
  (cl-ulong :uint64))

(defctypes
  (cl-half :uint16)
  (cl-float :float)
  (cl-double :double))

(defctypes
  (size-t :ulong)
  (intptr-t :long)
  (uintptr-t :ulong))

#| cl.h |#
(defctypes
  (cl-platform-id :pointer)
  (cl-device-id :pointer)
  (cl-context :pointer)
  (cl-command-queue :pointer)
  (cl-mem :pointer)
  (cl-program :pointer)
  (cl-kernel :pointer)
  (cl-event :pointer)
  (cl-sampler :pointer))

(defctypes
  (cl-bool cl-uint)
  (cl-bitfield cl-ulong)
  (cl-device-type cl-bitfield)
  (cl-platform-info cl-uint)
  (cl-device-info cl-uint)
  (cl-device-fp-config cl-bitfield)
  (cl-device-mem-cache-type cl-uint)
  (cl-device-local-mem-type cl-uint)
  (cl-device-exec-capabilities cl-bitfield)
  (cl-command-queue-properties cl-bitfield)
  (cl-device-partition-property intptr-t)
  (cl-device-affinity-domain cl-bitfield))

(defctypes
  (cl-context-properties intptr-t)
  (cl-context-info cl-uint)
  (cl-command-queue-info cl-uint)
  (cl-channel-order cl-uint)
  (cl-channel-type cl-uint)
  (cl-mem-flags cl-bitfield)
  (cl-mem-object-type cl-uint)
  (cl-mem-info cl-uint)
  (cl-mem-migration-flags cl-bitfield)
  (cl-image-info cl-uint)
  (cl-buffer-create-type cl-uint)
  (cl-addressing-mode cl-uint)
  (cl-filter-mode cl-uint)
  (cl-sampler-info cl-uint)
  (cl-map-flags cl-bitfield)
  (cl-program-info cl-uint)
  (cl-program-build-info cl-uint)
  (cl-program-binary-type cl-uint)
  (cl-build-status cl-int)
  (cl-kernel-info cl-uint)
  (cl-kernel-arg-info cl-uint)
  (cl-kernel-arg-address-qualifier cl-uint)
  (cl-kernel-arg-access-qualifier cl-uint)
  (cl-kernel-arg-type-qualifier cl-bitfield)
  (cl-kernel-work-group-info cl-uint)
  (cl-event-info cl-uint)
  (cl-command-type cl-uint)
  (cl-profiling-info cl-uint))

(defcstruct-export cl-image-format
  (image-channel-order cl-channel-order)
  (image-channel-data-type cl-channel-type))

(defcstruct-export cl-image-desc
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

(defcstruct-export cl-buffer-region
  (origin size-t)
  (size size-t))
