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
        :cl-oclapi.constants
        :cl-oclapi.functions
        :cl-oclapi.helpers.safe-call))
(in-package :cl-oclapi.helpers.with)

(annot:enable-annot-syntax)

#| Platform APIs |#

@export
(defmacro with-platform-ids ((platform-ids) &body body)
  (with-gensyms (num-platforms num platforms)
    `(with-foreign-object (,num-platforms 'cl-uint)
       (get-platform-ids 0 (null-pointer) ,num-platforms)
       (let ((,num (mem-aref ,num-platforms 'cl-uint)))
         (with-foreign-object (,platforms 'cl-platform-id ,num)
           (get-platform-ids ,num ,platforms ,num-platforms)
           (let ((,platform-ids (loop for n from 0 below ,num
                                      collecting (mem-aref ,platforms 'cl-platform-id n))))
             ,@body))))))

@export
(defmacro with-platform-id ((platform-id) &body body)
  (with-gensyms (platform-ids)
    `(with-platform-ids (,platform-ids)
       (let ((,platform-id (car ,platform-ids)))
         ,@body))))

#| Device APIs |#

@export
(defmacro with-device-ids ((devices num-devices platform &key (device-type +cl-device-type-default+)) &body body)
  (with-gensyms (num)
    `(with-foreign-object (,num-devices 'cl-uint)
      (get-device-ids ,platform ,device-type 0 (null-pointer) ,num-devices)
      (let ((,num (mem-aref ,num-devices 'cl-uint)))
        (with-foreign-object (,devices 'cl-device-id ,num)
          (get-device-ids ,platform ,device-type ,num ,devices ,num-devices)
          (progn
            ,@body))))))

#| Context APIs |#

@export
(defmacro with-context ((context properties num-devices devices &optional pfn-notify user-data) &body body)
  `(let ((,context (create-context ,properties
                                   ,num-devices
                                   ,devices
                                   (or ,pfn-notify (null-pointer))
                                   (or ,user-data (null-pointer)))))
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
(defmacro with-buffer ((name context flags size &optional host-ptr) &body body)
  `(let ((,name (create-buffer ,context
                               ,flags
                               ,size
                               (or ,host-ptr (null-pointer)))))
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
(defmacro with-program-with-source ((program context count source &optional lengths) &body body)
  (with-gensyms (string pointer)
    `(with-foreign-string (,string ,source)
       (with-foreign-object (,pointer :pointer)
         (setf (mem-aref ,pointer :pointer) ,string)
         (let ((,program (create-program-with-source ,context
                                                     ,count
                                                     ,pointer
                                                     (or ,lengths (null-pointer)))))
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
  `(with-foreign-object (,name 'cl-size 3)
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

@export
(defmacro with-pointer ((name buffer) &body body)
  `(with-foreign-object (,name :pointer)
     (setf (mem-aref ,name :pointer) ,buffer)
     (progn
       ,@body)))

@export
(defmacro with-pointers (bindings &body body)
  (if bindings
      `(with-pointer ,(car bindings)
         (with-pointers ,(cdr bindings)
           ,@body))
      `(progn
         ,@body)))
