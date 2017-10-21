#|
  This file is a part of cl-oclapi project.
  Copyright (c) 2015 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-oclapi.functions
  (:use :cl
        :cffi
        :cl-annot
        :cl-oclapi.types))
(in-package :cl-oclapi.functions)

(annot:enable-annot-syntax)

#| cl.h - platform API. |#

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clGetPlatformIDs" cl-get-platform-ids) cl-int
  (num-entries cl-uint)
  (platforms (:pointer cl-platform-id))
  (num-platforms (:pointer cl-uint)))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clGetPlatformInfo" cl-get-platform-info) cl-int
  (platform cl-platform-id)
  (param-name cl-platform-info)
  (param-value-size cl-size)
  (param-value :pointer)
  (param-value-size-ret (:pointer cl-size)))

#| cl.h - Device APIs |#

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clGetDeviceIDs" cl-get-device-ids) cl-int
  (platform cl-platform-id)
  (device-type cl-device-type)
  (num-entries cl-uint)
  (devices (:pointer cl-device-id))
  (num-devices (:pointer cl-uint)))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clGetDeviceInfo" cl-get-device-info) cl-int
  (device cl-device-id)
  (param-name cl-device-info)
  (param-value-size cl-size)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer cl-size)))

;; CL_API_SUFFIX__VERSION_1_2;
@export
(defcfun ("clCreateSubDevices" cl-create-sub-devices) cl-int
  (in-device cl-device-id)
  (properties (:pointer cl-device-partition-property))
  (num-devices cl-uint)
  (out-devices (:pointer cl-device-id))
  (num-devices-ret (:pointer cl-uint)))

;; CL_API_SUFFIX__VERSION_1_2;
@export
(defcfun ("clRetainDevice" cl-retain-device) cl-int
  (device cl-device-id))

;; CL_API_SUFFIX__VERSION_1_2;
@export
(defcfun ("clReleaseDevice" cl-release-device) cl-int
  (device cl-device-id))

#| cl.h - Context APIs |#
;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clCreateContext" cl-create-context) cl-context
  (properties (:pointer cl-context-properties))
  (num-devices cl-uint)
  (devices (:pointer cl-device-id))
  (pfn-notify :pointer) ; void (CL_CALLBACK *)(const char *, const void *, size_t, void *),
  (user-data (:pointer :void))
  (errcode-ret (:pointer cl-int)))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clCreateContextFromType" cl-create-context-from-type) cl-context
  (properties (:pointer cl-context-properties))
  (device-type cl-device-type)
  (pfn-notify :pointer) ; void (CL_CALLBACK *)(const char *, const void *, size_t, void *),
  (user-data (:pointer :void))
  (errcode-ret (:pointer cl-int)))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clRetainContext" cl-retain-context) cl-int
  (context cl-context))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clReleaseContext" cl-release-context) cl-int
  (context cl-context))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clGetContextInfo" cl-get-context-info) cl-int
  (context cl-context)
  (param-name cl-context-info)
  (param-value-size cl-size)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer cl-size)))

#| cl.h - Command Queue APIs |#

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clCreateCommandQueue" cl-create-command-queue) cl-command-queue
  (context cl-context)
  (device cl-device-id)
  (properties cl-command-queue-properties)
  (errcode-ret (:pointer cl-int)))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clRetainCommandQueue" cl-retain-command-queue) cl-int
  (command-queue cl-command-queue))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clReleaseCommandQueue" cl-release-command-queue) cl-int
  (command-queue cl-command-queue))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clGetCommandQueueInfo" cl-get-command-queue-info) cl-int
  (command-queue cl-command-queue)
  (param-name cl-command-queue-info)
  (param-value-size cl-size)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer cl-size)))

#| Memory Object APIs |#

;; CL-API-SUFFIX--VERSION-1-0;
@export
(defcfun ("clCreateBuffer" cl-create-buffer) cl-mem
  (context cl-context)
  (flags cl-mem-flags)
  (size cl-size)
  (host-ptr :pointer)
  (errcode-ret (:pointer cl-int)))

;; CL-API-SUFFIX--VERSION-1-1;
@export
(defcfun ("clCreateSubBuffer" cl-create-sub-buffer) cl-mem
  (buffer cl-mem)
  (flags cl-mem-flags)
  (buffer-create-type cl-buffer-create-type)
  (buffer-create-info :pointer)
  (errcode-ret (:pointer cl-int)))

;; CL-API-SUFFIX--VERSION-1-2;
@export
(defcfun ("clCreateImage" cl-create-image) cl-mem
  (context cl-context)
  (flags cl-mem-flags)
  (image-format (:pointer cl-image-format))
  (image-desc (:pointer cl-image-desc))
  (host-ptr :pointer)
  (errcode-ret (:pointer cl-int)))

;; CL-API-SUFFIX--VERSION-1-0;
@export
(defcfun ("clRetainMemObject" cl-retain-mem-object) cl-int
  (memobj cl-mem))

;; CL-API-SUFFIX--VERSION-1-0;
@export
(defcfun ("clReleaseMemObject" cl-release-mem-object) cl-int
  (memobj cl-mem))

;; CL-API-SUFFIX--VERSION-1-0;
@export
(defcfun ("clGetSupportedImageFormats" cl-get-supported-image-formats) cl-int
  (context cl-context)
  (flags cl-mem-flags)
  (image-type cl-mem-object-type)
  (num-entries cl-uint)
  (image-formats (:pointer cl-image-format))
  (num-image-formats (:pointer cl-uint)))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clGetMemObjectInfo" cl-get-mem-object-info) cl-int
  (memobj cl-mem)
  (param-name cl-mem-info)
  (param-value-size cl-size)
  (param-value :pointer)
  (param-value-size-ret (:pointer cl-size)))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clGetImageInfo" cl-get-image-info) cl-int
  (image cl-mem)
  (param-name cl-image-info)
  (param-value-size cl-size)
  (param-value :pointer)
  (param-value-size-ret (:pointer cl-size)))

;; CL_API_SUFFIX__VERSION_1_1;
@export
(defcfun ("clSetMemObjectDestructorCallback" cl-set-mem-object-destruction-callback) cl-int
  (memobj cl-mem)
  (pfn-notify :pointer)
  (user-data :pointer))

#| Sampler APIs |#

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clCreateSampler" cl-create-sampler) cl-sampler
  (context cl-context)
  (normalized-coords cl-bool)
  (addressing-mode cl-filter-mode)
  (filter-mode cl-addressing-mode)
  (errcode-ret (:pointer cl-int)))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clRetainSampler" cl-retain-sampler) cl-int
  (sampler cl-sampler))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clReleaseSampler" cl-release-sampler) cl-int
  (sampler cl-sampler))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clGetSamplerInfo" cl-get-sampler-info) cl-int
  (sampler cl-sampler)
  (param-name cl-sampler-info)
  (param-value-size cl-size)
  (param-value :pointer)
  (param-value-size-ret (:pointer cl-size)))

#| Program Object APIs  |#

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clCreateProgramWithSource" cl-create-program-with-source) cl-program
  (context cl-context)
  (count cl-uint)
  (strings (:pointer (:pointer cl-char)))
  (lengths (:pointer cl-size))
  (errcode-ret (:pointer cl-int)))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clCreateProgramWithBinary" cl-create-program-with-binary) cl-program
  (context cl-context)
  (num-devices cl-uint)
  (device-list (:pointer cl-device-id))
  (lengths (:pointer cl-size))
  (binaries (:pointer (:pointer cl-char)))
  (binary-status (:pointer cl-int))
  (errcode-ret (:pointer cl-int)))

;; CL_API_SUFFIX__VERSION_1_2;
@export
(defcfun ("clCreateProgramWithBuiltInKernels" cl-create-program-with-built-in-kernels) cl-program
  (context cl-context)
  (num-devices cl-uint)
  (device-list (:pointer cl-device-id))
  (kernel-names (:pointer cl-char))
  (errcode-ret (:pointer cl-int)))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clRetainProgram" cl-retain-program) cl-int
  (program cl-program))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clReleaseProgram" cl-release-program) cl-int
  (program cl-program))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clBuildProgram" cl-build-program) cl-int
  (program cl-program)
  (num-deviceis cl-uint)
  (device-list (:pointer cl-device-id))
  (options (:pointer cl-char))
  (cl-callback :pointer) ;; void (CL_CALLBACK *  /* pfn_notify */)(cl_program /* program */, void * /* user_data */),
  (user-data (:pointer :void)))

;; CL_API_SUFFIX__VERSION_1_2;
@export
(defcfun ("clCompileProgram" cl-compile-program) cl-int
  (program cl-program)
  (num-deviceis cl-uint)
  (device-list (:pointer cl-device-id))
  (options (:pointer cl-char))
  (num-input-headers cl-uint)
  (input-headers (:pointer cl-program))
  (header-include-names (:pointer (:pointer cl-char)))
  (cl-callback :pointer) ;; void (CL_CALLBACK *  /* pfn_notify */)(cl_program /* program */, void * /* user_data */),
  (user-data (:pointer :void)))

;; CL_API_SUFFIX__VERSION_1_2;
@export
(defcfun ("clLinkProgram" cl-link-program) cl-program
  (context cl-context)
  (num-devices cl-uint)
  (device-list (:pointer cl-device-id))
  (options (:pointer cl-char))
  (num-input-programs cl-uint)
  (input-programs (:pointer cl-program))
  (cl-callback :pointer) ;; void (CL_CALLBACK *  pfn_notify */)(cl_program /* program */, void * /* user_data */),
  (user-data (:pointer :void)))

;; CL_API_SUFFIX__VERSION_1_2;
@export
(defcfun ("clUnloadPlatformCompiler" cl-unload-platform-compiler) cl-int
  (platform cl-platform-id))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clGetProgramInfo" cl-get-program-info) cl-int
  (program cl-program)
  (param-name cl-program-info)
  (param-value-size cl-size)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer cl-size)))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clGetProgramBuildInfo" cl-get-program-build-info) cl-int
  (program cl-program)
  (device cl-device-id)
  (param-name cl-program-build-info)
  (param-value-size cl-size)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer cl-size)))

#| Kernel Object APIs |#

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clCreateKernel" cl-create-kernel) cl-kernel
  (program cl-program)
  (kernel-name (:pointer :char))
  (errcode-ret (:pointer cl-int)))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clCreateKernelsInProgram" cl-create-kernels-in-program) cl-int
  (program cl-program)
  (num-kernels cl-uint)
  (kernels (:pointer cl-kernel))
  (num-kernels-ret (:pointer cl-uint)))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clRetainKernel" cl-retain-kernel) cl-int
  (kernel cl-kernel))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clReleaseKernel" cl-release-kernel) cl-int
  (kernel cl-kernel))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clSetKernelArg" cl-set-kernel-arg) cl-int
  (kernel cl-kernel)
  (arg-index cl-uint)
  (arg-size cl-size)
  (arg-value (:pointer :void)))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clGetKernelInfo" cl-get-kernel-info) cl-int
  (kernel cl-kernel)
  (param-name cl-kernel-info)
  (param-value-size cl-size)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer cl-size)))

;; CL_API_SUFFIX__VERSION_1_2;
@export
(defcfun ("clGetKernelArgInfo" cl-get-kernel-arg-info) cl-int
  (kernel cl-kernel)
  (arg-indx cl-uint)
  (param-name cl-kernel-arg-info)
  (param-value-size cl-size)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer cl-size)))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clGetKernelWorkGroupInfo" cl-get-kernel-work-group-info) cl-int
  (kernel cl-kernel)
  (device cl-device-id)
  (param-name cl-kernel-work-group-info)
  (param-value-size cl-size)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer cl-size)))

#| Event Object APIs |#

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clWaitForEvents" cl-wait-for-events) cl-int
  (num-events cl-uint)
  (event-list (:pointer cl-event)))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clGetEventInfo" cl-get-event-info) cl-int
  (event cl-event)
  (param-name cl-event-info)
  (param-value-size cl-size)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer cl-size)))

;; CL_API_SUFFIX__VERSION_1_1;
@export
(defcfun ("clCreateUserEvent" cl-create-user-event) cl-event
  (context cl-context)
  (errcode-ret (:pointer cl-int)))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clRetainEvent" cl-retain-event) cl-int
  (event cl-event))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clReleaseEvent" cl-release-event) cl-int
  (event cl-event))

;; CL_API_SUFFIX__VERSION_1_1;
@export
(defcfun ("clSetUserEventStatus" cl-set-user-event-status) cl-int
  (event cl-event)
  (execution-status cl-int))

;; CL_API_SUFFIX__VERSION_1_1;
@export
(defcfun ("clSetEventCallback" cl-set-event-callback) cl-int
  (event cl-event)
  (command-exec-callback-type cl-int)
  (pfn-notify :pointer)
  (user-data :pointer))

#| Profiling APIs |#

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clGetEventProfilingInfo" cl-get-event-profiling-info) cl-int
  (event cl-event)
  (param-name cl-profiling-info)
  (param-value-size cl-size)
  (param-value (:pointer :void))
  (param-value-size-ret (:pointer cl-size)))

#| Flush and Finish APIs |#

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clFlush" cl-flush) cl-int
  (command-queue cl-command-queue))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clFinish" cl-finish) cl-int
  (command-queue cl-command-queue))

#| Enqueued Commands APIs |#

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clEnqueueReadBuffer" cl-enqueue-read-buffer) cl-int
  (command-queue cl-command-queue)
  (buffer cl-mem)
  (blocking-read cl-bool)
  (offset cl-size)
  (size cl-size)
  (ptr (:pointer :void))
  (num-events-in-wait-list cl-uint)
  (event-wait-list cl-event)
  (event cl-event))

;; CL_API_SUFFIX__VERSION_1_1;
@export
(defcfun ("clEnqueueReadBufferRect" cl-enqueue-read-buffer-rect) cl-int
  (command-queue cl-command-queue)
  (buffer cl-mem)
  (blocking-read cl-bool)
  (buffer-offset (:pointer cl-size))
  (host-offset (:pointer cl-size))
  (region (:pointer cl-size))
  (buffer-row-pitch cl-size)
  (buffer-slice-pitch cl-size)
  (host-row-pitch cl-size)
  (host-slice-pitch cl-size)
  (ptr :pointer :void)
  (num-events-in-wait-list cl-uint)
  (event-wait-list cl-event)
  (event cl-event))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clEnqueueWriteBuffer" cl-enqueue-write-buffer) cl-int
  (command-queue cl-command-queue)
  (buffer cl-mem)
  (blocking-write cl-bool)
  (offset cl-size)
  (size cl-size)
  (ptr (:pointer :void))
  (num-events-in-wait-list cl-uint)
  (event-wait-list cl-event)
  (event cl-event))

;; CL_API_SUFFIX__VERSION_1_1;
@export
(defcfun ("clEnqueueWriteBufferRect" cl-enqueue-write-buffer-rect) cl-int
  (command-queue cl-command-queue)
  (buffer cl-mem)
  (blocking-write cl-bool)
  (buffer-offset (:pointer cl-size))
  (host-offset (:pointer cl-size))
  (region (:pointer cl-size))
  (buffer-row-pitch cl-size)
  (buffer-slice-pitch cl-size)
  (host-row-pitch cl-size)
  (host-slice-pitch cl-size)
  (ptr (:pointer :void))
  (num-events-in-wait-list cl-uint)
  (event-wait-list cl-event)
  (event cl-event))

;; CL_API_SUFFIX__VERSION_1_2;
@export
(defcfun ("clEnqueueFillBuffer" cl-enqueue-fill-buffer) cl-int
  (command-queue cl-command-queue)
  (buffer cl-mem)
  (pattern (:pointer :void))
  (pattern-size cl-size)
  (offset cl-size)
  (size cl-size)
  (num-events-in-wait-list cl-uint)
  (event-wait-list cl-event)
  (event cl-event))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clEnqueueCopyBuffer" cl-enqueue-copy-buffer) cl-int
  (command-queue cl-command-queue)
  (src-buffer cl-mem)
  (dst-buffer cl-mem)
  (src-offset cl-size)
  (dst-offset cl-size)
  (size cl-size)
  (num-events-in-wait-list cl-uint)
  (event-wait-list cl-event)
  (event cl-event))

;; CL_API_SUFFIX__VERSION_1_1;
@export
(defcfun ("clEnqueueCopyBufferRect" cl-enqueue-copy-buffer-rect) cl-int
  (command-queue cl-command-queue)
  (src-buffer cl-mem)
  (dst-buffer cl-mem)
  (src-origin (:pointer cl-size))
  (dst-origin (:pointer cl-size))
  (region (:pointer cl-size))
  (src-row-pitch cl-size)
  (src-slice-pitch cl-size)
  (dst-row-pitch cl-size)
  (dst-slice-pitch cl-size)
  (num-events-in-wait-list cl-uint)
  (event-wait-list cl-event)
  (event cl-event))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clEnqueueReadImage" cl-enqueue-read-image) cl-int
  (command-queue cl-command-queue)
  (image cl-mem)
  (blocking-read cl-bool)
  (origin (:pointer cl-size))
  (region (:pointer cl-size))
  (row-pitch cl-size)
  (slice-pitch cl-size)
  (ptr (:pointer :void))
  (num-events-in-wait-list cl-uint)
  (event-wait-list cl-event)
  (event cl-event))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clEnqueueWriteImage" cl-enqueue-write-image) cl-int
  (command-queue cl-command-queue)
  (image cl-mem)
  (blocking-write cl-bool)
  (origin (:pointer cl-size))
  (region (:pointer cl-size))
  (input-row-pitch cl-size)
  (input-slice-pitch cl-size)
  (ptr (:pointer :void))
  (num-events-in-wait-list cl-uint)
  (event-wait-list cl-event)
  (event cl-event))

;; CL_API_SUFFIX__VERSION_1_2;
@export
(defcfun ("clEnqueueFillImage" cl-enqueue-fill-image) cl-int
  (command-queue cl-command-queue)
  (image cl-mem)
  (fill-color (:pointer :void))
  (origin (:pointer cl-size))
  (region (:pointer cl-size))
  (num-events-in-wait-list cl-uint)
  (event-wait-list cl-event)
  (event cl-event))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clEnqueueCopyImage" cl-enqueue-copy-image) cl-int
  (command-queue cl-command-queue)
  (src-image cl-mem)
  (dst-image cl-mem)
  (src-origin (:pointer cl-size))
  (dst-origin (:pointer cl-size))
  (region (:pointer cl-size))
  (num-events-in-wait-list cl-uint)
  (event-wait-list cl-event)
  (event cl-event))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clEnqueueCopyImageToBuffer" cl-enqueue-copy-image-to-buffer) cl-int
  (command-queue cl-command-queue)
  (src-image cl-mem)
  (dst-buffer cl-mem)
  (src-origin (:pointer cl-size))
  (region (:pointer cl-size))
  (dst-offset cl-size)
  (num-events-in-wait-list cl-uint)
  (event-wait-list cl-event)
  (event cl-event))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clEnqueueCopyBufferToImage" cl-enqueue-copy-buffer-to-image) cl-int
  (command-queue cl-command-queue)
  (src-buffer cl-mem)
  (dst-image cl-mem)
  (src-offset cl-size)
  (dst-origin (:pointer cl-size))
  (region (:pointer cl-size))
  (num-events-in-wait-list cl-uint)
  (event-wait-list cl-event)
  (event cl-event))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clEnqueueMapBuffer" cl-enqueue-map-buffer) (:pointer :void)
  (command-queue cl-command-queue)
  (buffer cl-mem)
  (blocking-map cl-bool)
  (map-flags cl-map-flags)
  (offset cl-size)
  (size cl-size)
  (num-events-in-wait-list cl-uint)
  (event-wait-list cl-event)
  (event cl-event)
  (errcode-ret (:pointer cl-int)))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clEnqueueMapImage" cl-enqueue-map-image) (:pointer :void)
  (command-queue cl-command-queue)
  (image cl-mem)
  (blocking-map cl-bool)
  (map-flags cl-map-flags)
  (origin (:pointer cl-size))
  (region (:pointer cl-size))
  (image-row-pitch (:pointer cl-size))
  (image-slice-pitch (:pointer cl-size))
  (num-events-in-wait-list cl-uint)
  (event-wait-list cl-event)
  (event cl-event)
  (errcode-ret (:pointer cl-int)))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clEnqueueUnmapMemObject" cl-enqueue-unmap-mem-object) cl-int
  (command-queue cl-command-queue)
  (memobj cl-mem)
  (mapped-ptr (:pointer :void))
  (num-events-in-wait-list cl-uint)
  (event-wait-list cl-event)
  (event cl-event))

;; CL_API_SUFFIX__VERSION_1_2;
@export
(defcfun ("clEnqueueMigrateMemObjects" cl-enqueue-migrate-mem-objects) cl-int
  (command-queue cl-command-queue)
  (num-mem-objects cl-uint)
  (mem-objects (:pointer cl-mem))
  (flags cl-mem-migration-flags)
  (num-events-in-wait-list cl-uint)
  (event-wait-list cl-event)
  (event cl-event))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clEnqueueNDRangeKernel" cl-enqueue-ndrange-kernel) cl-int
  (command-queue cl-command-queue)
  (kernel cl-kernel)
  (work-dim cl-uint)
  (global-work-offset (:pointer cl-size))
  (global-work-size (:pointer cl-size))
  (local-work-size (:pointer cl-size))
  (num-events-in-wait-list cl-uint)
  (event-wait-list cl-event)
  (event cl-event))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clEnqueueTask" cl-enqueue-task) cl-int
  (command-queue cl-command-queue)
  (kernel cl-kernel)
  (num-events-in-wait-list cl-uint)
  (event-wait-list cl-event)
  (event cl-event))

;; CL_API_SUFFIX__VERSION_1_0;
@export
(defcfun ("clEnqueueNativeKernel" cl-enqueue-native-kernel) cl-int
  (command-queue cl-command-queue)
  (user-func :pointer) ;; void (CL_CALLBACK * /*user_func*/)(void *),
  (args (:pointer :void))
  (cb-args cl-size)
  (num-mem-objects cl-uint)
  (mem-list (:pointer cl-mem))
  (args-mem-loc (:pointer (:pointer :void)))
  (num-events-in-wait-list cl-uint)
  (event-wait-list cl-event)
  (event cl-event))

;; CL_API_SUFFIX__VERSION_1_2;
@export
(defcfun ("clEnqueueMarkerWithWaitList" cl-enqueue-markerwith-wait-list) cl-int
  (command-queue cl-command-queue)
  (num-events-in-wait-list cl-uint)
  (event-wait-list cl-event)
  (event cl-event))

;; CL_API_SUFFIX__VERSION_1_2;
@export
(defcfun ("clEnqueueBarrierWithWaitList" cl-enqueue-barrier-with-wait-list) cl-int
  (command-queue cl-command-queue)
  (num-events-in-wait-list cl-uint)
  (event-wait-list cl-event)
  (event cl-event))

