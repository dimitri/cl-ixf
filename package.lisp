(defpackage #:ixf
  (:use :cl)
  (:import-from #:split-sequence #:split-sequence)
  (:export #:*ixf-stream*
           #:with-ixf-file
           #:read-headers
           #:map-data
           #:read-data
           #:read-ixf-file

           ;; header structures
           #:ixf-header
           #:ixf-header-date
           #:ixf-header-time
           #:ixf-header-count
           #:ixf-header-code-page
           #:ixf-header-encoding

           #:ixf-column
           #:ixf-column-name
           #:ixf-column-nullable
           #:ixf-column-has-default
           #:ixf-column-default
           #:ixf-column-pkey-pos
           #:ixf-column-type
           #:ixf-column-desc

           #:ixf-table
           #:ixf-table-name
           #:ixf-table-creator
           #:ixf-table-source
           #:ixf-table-ncol
           #:ixf-table-columns
           #:ixf-table-pkey-name
           #:ixf-table-desc

           #:ixf-file
           #:ixf-file-header
           #:ixf-file-table

           ;; data types
           #:+bigint+
           #:+blob+
           #:+clob+
           #:+blob-file+
           #:+clob-file+
           #:+dbclob-file+
           #:+char+
           #:+date+
           #:+dbclob+
           #:+decimal+
           #:+decfloat+
           #:+float+
           #:+graphic+
           #:+integer+
           #:+longvarchar+
           #:+longvargraphic+
           #:+smallint+
           #:+time+
           #:+timestamp+
           #:+varchar+
           #:+vargraphic+))
