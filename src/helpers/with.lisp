#|
  This file is a part of cl-oclapi project.
  Copyright (c) 2016 gos-k (mag4.elan@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-oclapi.helpers.with
  (:use :cl
        :alexandria
        :cffi
        :cl-annot
        :cl-oclapi.types
        :cl-oclapi.helpers.safe-call))
(in-package :cl-oclapi.helpers.with)

(annot:enable-annot-syntax)

#| Context APIs |#

@export
(defmacro with-context ((context
                         properties
                         num-devices
                         devices
                         &optional
                           (pfn-notify (null-pointer))
                           (user-data (null-pointer)))
                        &body body)
  `(let ((,context (create-context ,properties
                                   ,num-devices
                                   ,devices
                                   ,pfn-notify
                                   ,user-data)))
     (unwind-protect
          (progn
            ,@body)
       (release-context ,context))))

#| Command Queue APIs |#

@export
(defmacro with-command-queue ((name context device properties) &body body)
  `(let ((,name (create-command-queue ,context ,device ,properties)))
     (unwind-protect
          (progn
            ,@body)
       (release-command-queue ,name))))

#| Memory Object APIs |#

@export
(defmacro with-buffer ((name context flags size &optional (host-ptr (null-pointer))) &body body)
  `(let ((,name (create-buffer ,context ,flags ,size ,host-ptr)))
     (unwind-protect
          (progn
            ,@body)
       (release-mem-object ,name))))

@export
(defmacro with-buffers (bindings &body body)
  (if bindings
      `(with-buffer ,(car bindings)
         (with-buffers ,(cdr bindings)
           ,@body))
      `(progn
         ,@body)))

#| Program Object APIs  |#

@export
(defmacro with-program-with-source ((program context count source &optional (lengths (null-pointer))) &body body)
  (with-gensyms (string pointer)
    `(with-foreign-string (,string ,source)
       (with-foreign-object (,pointer :pointer)
         (setf (mem-aref ,pointer :pointer) ,string)
         (let ((,program (create-program-with-source ,context ,count ,pointer ,lengths)))
           (unwind-protect
                (progn
                  ,@body)
             (release-program ,program)))))))

#| Kernel Object APIs |#

@export
(defmacro with-kernel ((name program kernel-name) &body body)
  (with-gensyms (foreign-name)
    `(let* ((,foreign-name (foreign-string-alloc ,kernel-name))
            (,name (create-kernel ,program ,foreign-name)))
       (unwind-protect
            (progn
              ,@body)
         (release-kernel ,name)
         (foreign-string-free ,foreign-name)))))

#| parameter setup |#

@export
(defmacro with-work-size ((name x &optional (y 0) (z 0)) &body body)
  `(with-foreign-object (,name 'size-t 3)
     (set-work-size ,name ,x ,y ,z)
     (progn
       ,@body)))

@export
(defmacro with-work-sizes (bindings &body body)
  (if bindings
      `(with-work-size ,(car bindings)
         (with-work-sizes ,(cdr bindings)
           ,@body))
      `(progn
         ,@body)))
