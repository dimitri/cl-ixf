(defpackage #:ixf
  (:use :cl)
  (:import-from #:split-sequence #:split-sequence)
  (:export #:*ixf-stream*
           #:with-ixf-file
           #:read-headers
           #:map-data
           #:read-data
           #:read-ixf-file

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
