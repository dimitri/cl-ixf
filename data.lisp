;;;
;;; Read IBM ixf files data.
;;;

(in-package #:ixf)

(defun parse-ixf-data (data-type nullable pos length data)
  "Read data at given POSITION in DATA, with given LENGTH and DATA-TYPE."

  (unless (and nullable (parse-ixf-null data pos))
    (when nullable (setf pos (+ 2 pos)))
    (case data-type
      (#. +integer+   (parse-ixf-integer data pos))
      (#. +smallint+  (parse-ixf-smallint data pos))
      (#. +bigint+    (parse-ixf-bigint data pos))

      (#. +decimal+   (let* ((length    (format nil "~5,'0d" length))
                             (precision (parse-integer length :end 3))
                             (scale     (parse-integer length :start 3)))
                        (parse-ixf-decimal data pos precision scale)))

      (#. +float+     (parse-ixf-float data pos length))

      (#. +timestamp+ (parse-ixf-timestamp data pos length))
      (#. +time+      (parse-ixf-time data pos))
      (#. +date+      (parse-ixf-date data pos))

      (#. +char+      (parse-ixf-string data pos length))

      (#. +varchar+   (let ((length (parse-ixf-smallint data pos)))
                        ;; The current length indicators are 2-byte integers
                        ;; in a form specified by the IXFTMFRM field.
                        (parse-ixf-string data (+ pos 2) length))))))

(defmethod parse-data-record ((ixf ixf-file) record)
  "Parse given data record and return what we found."
  (let* ((header (ixf-file-header ixf))
         (table  (ixf-file-table ixf))
         (babel:*default-character-encoding* (ixf-header-encoding header)))
    (loop :with data := (get-record-property :IXFDCOLS record)
       :with record := (make-array (ixf-table-ncol table))
       :for i :below (ixf-table-ncol table)
       :for column :across (ixf-table-columns table)
       :do (setf (svref record i)
                 (let ((data-type (ixf-column-type column))
                       (length    (ixf-column-length column))
                       (pos       (- (ixf-column-pos column) 1))
                       (nullable  (ixf-column-nullable column)))
                   (parse-ixf-data data-type nullable pos length data)))
       :finally (return record))))
