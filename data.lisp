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

(defmethod maybe-read-record ((ixf ixf-file) (col ixf-column) d-id)
  "Compare current D-ID value with expected (ixf-column-d-id col) and read
   another record when they don't match"
  (cond ((= (ixf-column-d-id col) d-id)
         ;; column still in current record
         nil)

        ((= (ixf-column-d-id col) (+ 1 d-id))
         ;; now we need the next D record...
         (let ((next-record (read-next-record (ixf-file-stream ixf))))
           (assert (char= #\D (get-record-property :type next-record)))
           next-record))

        (t
         (error "Lost sync: current d-id is ~a, next column to be read on ~d."
                d-id (ixf-column-d-id col)))))

(defmethod read-next-row ((ixf ixf-file) first-record)
  "Read next IXF row: each row in the table is represented by one or more
   records, so keep reading D records as we need them."
  (let* ((header (ixf-file-header ixf))
         (table  (ixf-file-table ixf))
         (babel:*default-character-encoding* (ixf-header-encoding header)))
    (loop
       :with row := (make-array (ixf-table-ncol table))
       :for i :below (ixf-table-ncol table)
       :for column :across (ixf-table-columns table)

       :for record
       := first-record
       :then (or (maybe-read-record ixf column current-d-id) record)

       :for current-d-id := (get-record-property :IXFDRID record)
       :for data := (get-record-property :IXFDCOLS record)

       :do (setf (svref row i)
                 (let ((data-type (ixf-column-type column))
                       (length    (ixf-column-length column))
                       (pos       (- (ixf-column-pos column) 1))
                       (nullable  (ixf-column-nullable column)))
                   (parse-ixf-data data-type nullable pos length data)))
       :finally (return row))))
