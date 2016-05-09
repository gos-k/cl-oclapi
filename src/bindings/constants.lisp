#|
  This file is a part of cl-oclapi project.
  Copyright (c) 2015 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-oclapi.constants
  (:use :cl))
(in-package :cl-oclapi.constants)

(defmacro defconstants (&body constants)
  `(progn
     ,@(loop for constant in constants
             collect `(export ',(car constant))
             collect `(defconstant ,@constant))))

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

#| cl.h - cl_mem_flags - bitfield |#
(defconstants
  (+cl-mem-read-write+      #.(ash 1 0))
  (+cl-mem-write-only+      #.(ash 1 1))
  (+cl-mem-read-only+       #.(ash 1 2))
  (+cl-mem-use-host-ptr+    #.(ash 1 3))
  (+cl-mem-alloc-host-ptr+  #.(ash 1 4))
  (+cl-mem-copy-host-ptr+   #.(ash 1 5))
  ;; reserved (1 << 6)
  (+cl-mem-host-write-only+ #.(ash 1 7))
  (+cl-mem-host-read-only+  #.(ash 1 8))
  (+cl-mem-host-no-access+  #.(ash 1 9)))

#| cl.h - cl_mem_migration_flags - bitfield |#
(defconstants
  (+cl-migrate-mem-object-host+              #.(ash 1 0))
  (+cl-migrate-mem-object-content-undefined+ #.(ash 1 1)))

#| cl.h - cl_channel_order |#
(defconstants
  (+cl-r+             #x10b0)
  (+cl-a+             #x10b1)
  (+cl-rg+            #x10b2)
  (+cl-ra+            #x10b3)
  (+cl-rgb+           #x10b4)
  (+cl-rgba+          #x10b5)
  (+cl-bgra+          #x10b6)
  (+cl-argb+          #x10b7)
  (+cl-intensity+     #x10b8)
  (+cl-luminance+     #x10b9)
  (+cl-rx+            #x10ba)
  (+cl-rgx+           #x10bb)
  (+cl-rgbx+          #x10bc)
  (+cl-depth+         #x10bd)
  (+cl-depth-stencil+ #x10be))

#| cl.h - cl_channel_type |#
(defconstants
  (+cl-snorm-int8+       #x10d0)
  (+cl-snorm-int16+      #x10d1)
  (+cl-unorm-int8+       #x10d2)
  (+cl-unorm-int16+      #x10d3)
  (+cl-unorm-short-565+  #x10d4)
  (+cl-unorm-short-555+  #x10d5)
  (+cl-unorm-int-101010+ #x10d6)
  (+cl-signed-int8+      #x10d7)
  (+cl-signed-int16+     #x10d8)
  (+cl-signed-int32+     #x10d9)
  (+cl-unsigned-int8+    #x10da)
  (+cl-unsigned-int16+   #x10db)
  (+cl-unsigned-int32+   #x10dc)
  (+cl-half-float+       #x10dd)
  (+cl-float+            #x10de)
  (+cl-unorm-int24+      #x10df))

#| cl.h - cl_mem_object_type |#
(defconstants
  (+cl-mem-object-buffer+         #x10f0)
  (+cl-mem-object-image2d+        #x10f1)
  (+cl-mem-object-image3d+        #x10f2)
  (+cl-mem-object-image2d-array+  #x10f3)
  (+cl-mem-object-image1d+        #x10f4)
  (+cl-mem-object-image1d-array+  #x10f5)
  (+cl-mem-object-image1d-buffer+ #x10f6))

#| cl.h - cl_mem_info |#
(defconstants
  (+cl-mem-type+                 #x1100)
  (+cl-mem-flags+                #x1101)
  (+cl-mem-size+                 #x1102)
  (+cl-mem-host-ptr+             #x1103)
  (+cl-mem-map-count+            #x1104)
  (+cl-mem-reference-count+      #x1105)
  (+cl-mem-context+              #x1106)
  (+cl-mem-associated-memobject+ #x1107)
  (+cl-mem-offset+               #x1108)
  (+cl-mem-uses-svm-pointer+     #x1109))

#| cl.h - cl_image_info |#
(defconstants
  (+cl-image-format+         #x1110)
  (+cl-image-element-size+   #x1111)
  (+cl-image-row-pitch+      #x1112)
  (+cl-image-slice-pitch+    #x1113)
  (+cl-image-width+          #x1114)
  (+cl-image-height+         #x1115)
  (+cl-image-depth+          #x1116)
  (+cl-image-array-size+     #x1117)
  (+cl-image-buffer+         #x1118)
  (+cl-image-num-mip-levels+ #x1119)
  (+cl-image-num-samples+    #x111a))

#| cl.h - cl_addressing_mode |#
(defconstants
  (+cl-address-none+            #x1130)
  (+cl-address-clamp-to-edge+   #x1131)
  (+cl-address-clamp+           #x1132)
  (+cl-address-repeat+          #x1133)
  (+cl-address-mirrored-repeat+ #x1134))

#| cl.h - cl_filter_mode |#
(defconstants
  (+cl-filter-nearest+ #x1140)
  (+cl-filter-linear+  #x1141))

#| cl.h - cl_sampler_info |#
(defconstants
  (+cl-sampler-reference-count+   #x1150)
  (+cl-sampler-context+           #x1151)
  (+cl-sampler-normalized-coords+ #x1152)
  (+cl-sampler-addressing-mode+   #x1153)
  (+cl-sampler-filter-mode+       #x1154))

#| cl.h - cl_map_flags - bitfield |#
(defconstants
  (+cl-map-read+                    #.(ash 1 0))
  (+cl-map-write+                   #.(ash 1 1))
  (+cl-map-write-invalidate-region+ #.(ash 1 2)))

#| cl.h - cl_program_info |#
(defconstants
  (+cl-program-reference-count+ #x1160)
  (+cl-program-context+         #x1161)
  (+cl-program-num-devices+     #x1162)
  (+cl-program-devices+         #x1163)
  (+cl-program-source+          #x1164)
  (+cl-program-binary-sizes+    #x1165)
  (+cl-program-binaries+        #x1166)
  (+cl-program-num-kernels+     #x1167)
  (+cl-program-kernel-names+    #x1168))

#| cl.h - cl_program_build_info |#
(defconstants
  (+cl-program-build-status+  #x1181)
  (+cl-program-build-options+ #x1182)
  (+cl-program-build-log+     #x1183)
  (+cl-program-binary-type+   #x1184))

#| cl.h - cl_program_binary_type |#
(defconstants
  (+cl-program-binary-type-none+            #x0)
  (+cl-program-binary-type-compiled-object+ #x1)
  (+cl-program-binary-type-library+         #x2)
  (+cl-program-binary-type-executable+      #x4))

#| cl.h - cl_build_status |#
(defconstants
  (+cl-build-success+     0)
  (+cl-build-none+        -1)
  (+cl-build-error+       -2)
  (+cl-build-in-progress+ -3))

#| cl.h - cl_kernel_info |#
(defconstants
  (+cl-kernel-function-name+   #x1190)
  (+cl-kernel-num-args+        #x1191)
  (+cl-kernel-reference-count+ #x1192)
  (+cl-kernel-context+         #x1193)
  (+cl-kernel-program+         #x1194)
  (+cl-kernel-attributes+      #x1195))

#| cl.h - cl_kernel_arg_info |#
(defconstants
  (+cl-kernel-arg-address-qualifier+ #x1196)
  (+cl-kernel-arg-access-qualifier+  #x1197)
  (+cl-kernel-arg-type-name+         #x1198)
  (+cl-kernel-arg-type-qualifier+    #x1199)
  (+cl-kernel-arg-name+              #x119a))

#| cl.h - cl_kernel_arg_address_qualifier |#
(defconstants
  (+cl-kernel-arg-address-global+   #x119b)
  (+cl-kernel-arg-address-local+    #x119c)
  (+cl-kernel-arg-address-constant+ #x119d)
  (+cl-kernel-arg-address-private+  #x119e))

#| cl.h - cl_kernel_arg_access_qualifier |#
(defconstants
  (+cl-kernel-arg-access-read-only+  #x11a0)
  (+cl-kernel-arg-access-write-only+ #x11a1)
  (+cl-kernel-arg-access-read-write+ #x11a2)
  (+cl-kernel-arg-access-none+       #x11a3))

#| cl.h - cl_kernel_arg_type_qualifer |#
(defconstants
  (+cl-kernel-arg-type-none+     0)
  (+cl-kernel-arg-type-const+    #.(ash 1 0))
  (+cl-kernel-arg-type-restrict+ #.(ash 1 1))
  (+cl-kernel-arg-type-volatile+ #.(ash 1 2)))

#| cl.h - cl_kernel_work_group_info |#
(defconstants
  (+cl-kernel-work-group-size+                    #x11b0)
  (+cl-kernel-compile-work-group-size+            #x11b1)
  (+cl-kernel-local-mem-size+                     #x11b2)
  (+cl-kernel-preferred-work-group-size-multiple+ #x11b3)
  (+cl-kernel-private-mem-size+                   #x11b4)
  (+cl-kernel-global-work-size+                   #x11b5))

#| cl.h - cl_event_info  |#
(defconstants
  (+cl-event-command-queue+            #x11d0)
  (+cl-event-command-type+             #x11d1)
  (+cl-event-reference-count+          #x11d2)
  (+cl-event-command-execution-status+ #x11d3)
  (+cl-event-context+                  #x11d4))

#| cl.h - cl_command_type |#
(defconstants
  (+cl-command-ndrange-kernel+       #x11f0)
  (+cl-command-task+                 #x11f1)
  (+cl-command-native-kernel+        #x11f2)
  (+cl-command-read-buffer+          #x11f3)
  (+cl-command-write-buffer+         #x11f4)
  (+cl-command-copy-buffer+          #x11f5)
  (+cl-command-read-image+           #x11f6)
  (+cl-command-write-image+          #x11f7)
  (+cl-command-copy-image+           #x11f8)
  (+cl-command-copy-image-to-buffer+ #x11f9)
  (+cl-command-copy-buffer-to-image+ #x11fa)
  (+cl-command-map-buffer+           #x11fb)
  (+cl-command-map-image+            #x11fc)
  (+cl-command-unmap-mem-object+     #x11fd)
  (+cl-command-marker+               #x11fe)
  (+cl-command-acquire-gl-objects+   #x11ff)
  (+cl-command-release-gl-objects+   #x1200)
  (+cl-command-read-buffer-rect+     #x1201)
  (+cl-command-write-buffer-rect+    #x1202)
  (+cl-command-copy-buffer-rect+     #x1203)
  (+cl-command-user+                 #x1204)
  (+cl-command-barrier+              #x1205)
  (+cl-command-migrate-mem-objects+  #x1206)
  (+cl-command-fill-buffer+          #x1207)
  (+cl-command-fill-image+           #x1208))

#| cl.h - command execution status |#
(defconstants
  (+cl-complete+  #x0)
  (+cl-running+   #x1)
  (+cl-submitted+ #x2)
  (+cl-queued+    #x3))

#| cl.h - cl_buffer_create_type  |#
(defconstants
  (+cl-buffer-create-type-region+ #x1220))

#| cl.h - cl_profiling_info  |#
(defconstants
  (+cl-profiling-command-queued+ #x1280)
  (+cl-profiling-command-submit+ #x1281)
  (+cl-profiling-command-start+  #x1282)
  (+cl-profiling-command-end+    #x1283))
