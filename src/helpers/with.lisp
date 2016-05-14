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
        :cl-oclapi.functions
        :cl-oclapi.helpers.safe-call))
(in-package :cl-oclapi.helpers.with)

(annot:enable-annot-syntax)

#| Command Queue APIs |#

@export
(defmacro with-command-queue ((name context device properties) &body body)
  `(let ((,name (create-command-queue ,context ,device ,properties)))
     (unwind-protect
          (progn
            ,@body)
       (cl-release-command-queue ,name))))

#| Memory Object APIs |#

@export
(defmacro with-buffer ((name context flags size &optional (host-ptr (null-pointer))) &body body)
  `(let ((,name (create-buffer ,context ,flags ,size ,host-ptr)))
     (unwind-protect
          (progn
            ,@body)
       (cl-release-mem-object ,name))))

@export
(defmacro with-buffers (bindings &body body)
  (if bindings
      `(with-buffer ,(car bindings)
         (with-buffers ,(cdr bindings)
           ,@body))
      `(progn
         ,@body)))

#| Kernel Object APIs |#

@export
(defmacro with-kernel ((name program kernel-name) &body body)
  (with-gensyms (foreign-name)
    `(let* ((,foreign-name (foreign-string-alloc ,kernel-name))
            (,name (create-kernel ,program ,foreign-name)))
       (unwind-protect
            (progn
              ,@body)
         (cl-release-kernel ,name)
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
