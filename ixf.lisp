;;;
;;; API to read IBM ixf files.
;;;

(in-package #:ixf)

(defvar *ixf-stream* nil)

(defmacro with-ixf-file (filename &body body)
  "Executes BODY with *ixf-stream* bound to FILENAME stream contents."
  `(with-open-file (*ixf-stream* ,filename :element-type '(unsigned-byte 8))
     ,@body))

(defun read-headers (&optional (stream *ixf-stream*))
  "Return an IXF-FILE data structure filled with information read from FILENAME."
  (read-headers-from-stream stream))

(defun read-ixf-file (filename)
  "Docstring"
  (with-open-file (s filename :element-type '(unsigned-byte 8))
    (let ((length    (file-length s))
          (ixf       (read-headers s)))
      (values ixf
              (loop :while (< (file-position s) length)
                 :for record := (read-next-record s)
                 :when (char= #\D (get-record-property :type record))
                 :collect (parse-data-record ixf record))))))
