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

(defun map-data (ixf map-fn &optional (stream *ixf-stream*))
  "Call map-fn on each row of data read from STREAM given IXF definition."
  (loop :while (< (file-position stream) (file-length stream))
     :for record := (read-next-record stream)
     :when (char= #\D (get-record-property :type record))
     :do (funcall map-fn (parse-data-record ixf record))))

(defun read-data (ixf &optional (stream *ixf-stream*))
  "Return the data read from STREAM as a list of list of values."
  (loop :while (< (file-position stream) (file-length stream))
     :for record := (read-next-record stream)
     :when (char= #\D (get-record-property :type record))
     :collect (parse-data-record ixf record)))

(defun read-ixf-file (filename)
  "Docstring"
  (with-open-file (s filename :element-type '(unsigned-byte 8))
    (let ((ixf       (read-headers s)))
      (values ixf (read-data ixf s)))))

