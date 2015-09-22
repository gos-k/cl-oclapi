(in-package :cl-user)
(defpackage cl-oclapi
  (:use :cl :cffi)
  (:export :cl-uchar
           :cl-int
           :cl-uint
           :size-t
           :intptr-t
           :cl-platform-id
           :cl-device-id
           :cl-context-properties
           :cl-get-platform-ids
           :cl-get-platform-info
           :cl-get-device-ids
           :cl-get-device-info
           :cl-create-sub-devices
           :cl-retain-device
           :cl-release-device
           :cl-create-context
           :cl-create-context-from-type
           :cl-retain-context
           :cl-release-context
           :cl-get-context-info))
(in-package :cl-oclapi)

(define-foreign-library libopencl
  (t (:default "libOpenCL")))

(use-foreign-library libopencl)

(defmacro defctypes (&body types)
  `(progn
     ,@(loop for type in types
             collect `(defctype ,@type))))

(defmacro defconstants (&body constants)
  `(progn
     ,@(loop for constant in constants
             collect `(export ',(car constant))
             collect `(defconstant ,@constant))))

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

#| cl.h - error codes. |#

(defconstants
  (+cl-success+ 0)
  (+cl-device-not-found+ -1)
  (+cl-device-not-available+ -2)
  (+cl-compiler-not-available+ -3)
  (+cl-mem-object-allocation-failure+ -4)
  (+cl-out-of-resources+ -5)
  (+cl-out-of-host-memory+ -6)
  (+cl-profiling-info-not-available+ -7)
  (+cl-mem-copy-overlap+ -8)
  (+cl-image-format-mismatch+ -9)
  (+cl-image-format-not-supported+ -10)
  (+cl-build-program-failure+ -11)
  (+cl-map-failure+ -12)
  (+cl-misaligned-sub-buffer-offset+ -13)
  (+cl-exec-status-error-for-events-in-wait-list+ -14)
  (+cl-compile-program-failure+ -15)
  (+cl-linker-not-available+ -16)
  (+cl-link-program-failure+ -17)
  (+cl-device-partition-failed+ -18)
  (+cl-kernel-arg-info-not-available+ -19))

(defconstants
  (+cl-invalid-value+ -30)
  (+cl-invalid-device-type+ -31)
  (+cl-invalid-platform+ -32)
  (+cl-invalid-device+ -33)
  (+cl-invalid-context+ -34)
  (+cl-invalid-queue-properties+ -35)
  (+cl-invalid-command-queue+ -36)
  (+cl-invalid-host-ptr+ -37)
  (+cl-invalid-mem-object+ -38)
  (+cl-invalid-image-format-descriptor+ -39)
  (+cl-invalid-image-size+ -40)
  (+cl-invalid-sampler+ -41)
  (+cl-invalid-binary+ -42)
  (+cl-invalid-build-options+ -43)
  (+cl-invalid-program+ -44)
  (+cl-invalid-program-executable+ -45)
  (+cl-invalid-kernel-name+ -46)
  (+cl-invalid-kernel-definition+ -47)
  (+cl-invalid-kernel+ -48)
  (+cl-invalid-arg-index+ -49)
  (+cl-invalid-arg-value+ -50)
  (+cl-invalid-arg-size+ -51)
  (+cl-invalid-kernel-args+ -52)
  (+cl-invalid-work-dimension+ -53)
  (+cl-invalid-work-group-size+ -54)
  (+cl-invalid-work-item-size+ -55)
  (+cl-invalid-global-offset+ -56)
  (+cl-invalid-event-wait-list+ -57)
  (+cl-invalid-event+ -58)
  (+cl-invalid-operation+ -59)
  (+cl-invalid-gl-object+ -60)
  (+cl-invalid-buffer-size+ -61)
  (+cl-invalid-mip-level+ -62)
  (+cl-invalid-global-work-size+ -63)
  (+cl-invalid-property+ -64)
  (+cl-invalid-image-descriptor+ -65)
  (+cl-invalid-compiler-options+ -66)
  (+cl-invalid-linker-options+ -67)
  (+cl-invalid-device-partition-count+ -68))

#| cl.h - OpenCL version |#
(defconstants
  (+cl-version-1-0+ t)
  (+cl-version-1-1+ t)
  (+cl-version-1-2+ t))

#| cl.h - cl_bool |#
(defconstants
  (+cl-false+ nil)
  (+cl-true+ t)
  (+cl-blocking+ +cl-true+)
  (+cl-non-blocking+ +cl-false+))

#| cl.h - cl_platform_info |#
(defconstants
  (+cl-platform-profile+ #x0900)
  (+cl-platform-version+ #x0901)
  (+cl-platform-name+ #x0902)
  (+cl-platform-vendor+ #x0903)
  (+cl-platform-extensions+ #x0904))

#| cl.h - cl_device_type |#
(defconstants
  (+cl-device-type-default+     #b00001)
  (+cl-device-type-cpu+         #b00010)
  (+cl-device-type-gpu+         #b00100)
  (+cl-device-type-accelerator+ #b01000)
  (+cl-device-type-custom+      #b10000)
  (+cl-device-type-all+ #xffffffff))

#| cl.h - cl_device_info |#
(defconstants
  (+cl-device-type+                          #x1000)
  (+cl-device-vendor-id+                     #x1001)
  (+cl-device-max-compute-units+             #x1002)
  (+cl-device-max-work-item-dimensions+      #x1003)
  (+cl-device-max-work-group-size+           #x1004)
  (+cl-device-max-work-item-sizes+           #x1005)
  (+cl-device-preferred-vector-width-char+   #x1006)
  (+cl-device-preferred-vector-width-short+  #x1007)
  (+cl-device-preferred-vector-width-int+    #x1008)
  (+cl-device-preferred-vector-width-long+   #x1009)
  (+cl-device-preferred-vector-width-float+  #x100a)
  (+cl-device-preferred-vector-width-double+ #x100b)
  (+cl-device-max-clock-frequency+           #x100c)
  (+cl-device-address-bits+                  #x100d)
  (+cl-device-max-read-image-args+           #x100e)
  (+cl-device-max-write-image-args+          #x100f)
  (+cl-device-max-mem-alloc-size+            #x1010)
  (+cl-device-image2d-max-width+             #x1011)
  (+cl-device-image2d-max-height+            #x1012)
  (+cl-device-image3d-max-width+             #x1013)
  (+cl-device-image3d-max-height+            #x1014)
  (+cl-device-image3d-max-depth+             #x1015)
  (+cl-device-image-support+                 #x1016)
  (+cl-device-max-parameter-size+            #x1017)
  (+cl-device-max-samplers+                  #x1018)
  (+cl-device-mem-base-addr-align+           #x1019)
  (+cl-device-min-data-type-align-size+      #x101a)
  (+cl-device-single-fp-config+              #x101b)
  (+cl-device-global-mem-cache-type+         #x101c)
  (+cl-device-global-mem-cacheline-size+     #x101d)
  (+cl-device-global-mem-cache-size+         #x101e)
  (+cl-device-global-mem-size+               #x101f)
  (+cl-device-max-constant-buffer-size+      #x1020)
  (+cl-device-max-constant-args+             #x1021)
  (+cl-device-local-mem-type+                #x1022)
  (+cl-device-local-mem-size+                #x1023)
  (+cl-device-error-correction-support+      #x1024)
  (+cl-device-profiling-timer-resolution+    #x1025)
  (+cl-device-endian-little+                 #x1026)
  (+cl-device-available+                     #x1027)
  (+cl-device-compiler-available+            #x1028)
  (+cl-device-execution-capabilities+        #x1029)
  (+cl-device-queue-properties+              #x102a)
  (+cl-device-name+                          #x102b)
  (+cl-device-vendor+                        #x102c)
  (+cl-driver-version+                       #x102d)
  (+cl-device-profile+                       #x102e)
  (+cl-device-version+                       #x102f)
  (+cl-device-extensions+                    #x1030)
  (+cl-device-platform+                      #x1031)
  (+cl-device-double-fp-config+              #x1032)
  ;; 0x1033 reserved for CL_DEVICE_HALF_FP_CONFIG
  (+cl-device-preferred-vector-width-half+   #x1034)
  (+cl-device-host-unified-memory+           #x1035)
  (+cl-device-native-vector-width-char+      #x1036)
  (+cl-device-native-vector-width-short+     #x1037)
  (+cl-device-native-vector-width-int+       #x1038)
  (+cl-device-native-vector-width-long+      #x1039)
  (+cl-device-native-vector-width-float+     #x103a)
  (+cl-device-native-vector-width-double+    #x103b)
  (+cl-device-native-vector-width-half+      #x103c)
  (+cl-device-opencl-c-version+              #x103d)
  (+cl-device-linker-available+              #x103e)
  (+cl-device-built-in-kernels+              #x103f)
  (+cl-device-image-max-buffer-size+         #x1040)
  (+cl-device-image-max-array-size+          #x1041)
  (+cl-device-parent-device+                 #x1042)
  (+cl-device-partition-max-sub-devices+     #x1043)
  (+cl-device-partition-properties+          #x1044)
  (+cl-device-partition-affinity-domain+     #x1045)
  (+cl-device-partition-type+                #x1046)
  (+cl-device-reference-count+               #x1047)
  (+cl-device-preferred-interop-user-sync+   #x1048)
  (+cl-device-printf-buffer-size+            #x1049)
  (+cl-device-image-pitch-alignment+         #x104a)
  (+cl-device-image-base-address-alignment+  #x104b))

#| cl.h - cl_device_fp_config - bitfield |#
(defconstants
  (+cl-fp-denorm+                        #b00000001)
  (+cl-fp-inf-nan+                       #b00000010)
  (+cl-fp-round-to-nearest+              #b00000100)
  (+cl-fp-round-to-zero+                 #b00001000)
  (+cl-fp-round-to-inf+                  #b00010000)
  (+cl-fp-fma+                           #b00100000)
  (+cl-fp-soft-float+                    #b01000000)
  (+cl-fp-correctly-rounded-divide-sqrt+ #b10000000))

#| cl.h - cl_device_mem_cache_type |#
(defconstants
  (+cl-none+             #x0)
  (+cl-read-only-cache+  #x1)
  (+cl-read-write-cache+ #x2))

#| cl.h - cl_device_local_mem_type |#
(defconstants
  (+cl-local+ #x1)
  (+cl-global+ #x2))

#| cl.h - cl_device_exec_capabilities - bitfield |#
(defconstants
  (+cl-exec-kernel+ #.(ash 1 0))
  (+cl-exec-native-kernel+ #.(ash 1 1)))

#| cl.h - cl_command_queue_properties - bitfield |#
(defconstants
  (+cl-queue-out-of-order-exec-mode-enable+ #.(ash 1 0))
  (+cl-queue-profiling-enable+              #.(ash 1 1)))

#| cl.h - cl_context_info |#
(defconstants
  (+cl-context-reference-count+ #x1080)
  (+cl-context-devices+         #x1081)
  (+cl-context-properties+      #x1082)
  (+cl-context-num-devices+     #x1083))

#| cl.h - cl_context_properties |#
(defconstants
  (+cl-context-platform+          #x1084)
  (+cl-context-interop-user-sync+ #x1085))

#| cl.h - cl_device_partition_property |#
(defconstants
  (+cl-device-partition-equally+            #x1086)
  (+cl-device-partition-by-counts+          #x1087)
  (+cl-device-partition-by-counts-list-end+ #x0)
  (+cl-device-partition-by-affinity-domain+ #x1088))

#| cl.h - cl_device_affinity_domain |#
(defconstants
  (+cl-device-affinity-domain-numa+               #.(ash 1 0))
  (+cl-device-affinity-domain-l4-cache+           #.(ash 1 1))
  (+cl-device-affinity-domain-l3-cache+           #.(ash 1 2))
  (+cl-device-affinity-domain-l2-cache+           #.(ash 1 3))
  (+cl-device-affinity-domain-l1-cache+           #.(ash 1 4))
  (+cl-device-affinity-domain-next-partitionable+ #.(ash 1 5)))

#| cl.h - cl_command_queue_info |#
(defconstants
  (+cl-queue-context+         #x1090)
  (+cl-queue-device+          #x1091)
  (+cl-queue-reference-count+ #x1092)
  (+cl-queue-properties+      #x1093))

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

#| cl.h - Device APIs |#

;; CL_API_SUFFIX__VERSION_1_0;
(defcfun ("clGetDeviceIDs" cl-get-device-ids) cl-int
  (platform cl-platform-id)
  (device-type cl-device-type)
  (num-entries cl-uint)
  (devices (:pointer cl-device-id))
  (num-devices (:pointer cl-uint)))

;; CL_API_SUFFIX__VERSION_1_0;
(defcfun ("clGetDeviceInfo" cl-get-device-info) cl-int
  (device cl-device-id)
  (param-name cl-device-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))

;; CL_API_SUFFIX__VERSION_1_2;
(defcfun ("clCreateSubDevices" cl-create-sub-devices) cl-int
  (in-device cl-device-id)
  (properties (:pointer cl-device-partition-property))
  (num-devices cl-uint)
  (out-devices (:pointer cl-device-id))
  (num-devices-ret (:pointer cl-uint)))

;; CL_API_SUFFIX__VERSION_1_2;
(defcfun ("clRetainDevice" cl-retain-device) cl-int
  (device cl-device-id))

;; CL_API_SUFFIX__VERSION_1_2;
(defcfun ("clReleaseDevice" cl-release-device) cl-int
  (device cl-device-id))

#| cl.h - Context APIs |#
;; CL_API_SUFFIX__VERSION_1_0;
(defcfun ("clCreateContext" cl-create-context) cl-context
  (properties (:pointer cl-context-properties))
  (num-devices cl-uint)
  (devices (:pointer cl-device-id))
  (pfn-notify :pointer) ; void (CL_CALLBACK *)(const char *, const void *, size_t, void *),
  (user-data (:pointer :void))
  (errcode-ret (:pointer cl-int)))

;; CL_API_SUFFIX__VERSION_1_0;
(defcfun ("clCreateContextFromType" cl-create-context-from-type) cl-context
  (properties (:pointer cl-context-properties))
  (device-type cl-device-type)
  (pfn-notify :pointer) ; void (CL_CALLBACK *)(const char *, const void *, size_t, void *),
  (user-data (:pointer :void))
  (errcode-ret (:pointer cl-int)))

;; CL_API_SUFFIX__VERSION_1_0;
(defcfun ("clRetainContext" cl-retain-context) cl-int
  (context cl-context))

;; CL_API_SUFFIX__VERSION_1_0;
(defcfun ("clReleaseContext" cl-release-context) cl-int
  (context cl-context))

;; CL_API_SUFFIX__VERSION_1_0;
(defcfun ("clGetContextInfo" cl-get-context-info) cl-int
  (context cl-context)
  (param-name cl-context-info)
  (param-value-size size-t)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer size-t)))
