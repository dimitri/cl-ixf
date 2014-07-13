;;;
;;; Tools to handle IBM PC version of IXF file format
;;;
;;; http://www-01.ibm.com/support/knowledgecenter/SSEPGG_10.5.0/com.ibm.db2.luw.admin.dm.doc/doc/r0004669.html

(in-package :ixf)

(defconstant +bigint+          492)
(defconstant +blob+            404)
(defconstant +clob+            408)
(defconstant +blob-file+       916)
(defconstant +clob-file+       920)
(defconstant +dbclob-file+     924)
(defconstant +char+            452)
(defconstant +date+            384)
(defconstant +dbclob+          412)
(defconstant +decimal+         484)
(defconstant +decfloat+        996)
(defconstant +float+           480)
(defconstant +graphic+         468)
(defconstant +integer+         496)
(defconstant +longvarchar+     456)
(defconstant +longvargraphic+  472)
(defconstant +smallint+        500)
(defconstant +time+            388)
(defconstant +timestamp+       392)
(defconstant +varchar+         448)
(defconstant +vargraphic+      464)

(defun parse-ixf-null (data pos)
  "Read a NULL indicator and returns t when the value is NULL."
  ;;
  ;; The null indicator is a two-byte value set to x'0000' for not null, and
  ;; x'FFFF' for null.
  ;;
  (and (= #xff (aref data pos))
       (= #xff (aref data (+ 1 pos)))))

(defun parse-ixf-smallint (data pos)
  "Read a 2-byte integer."
  (logior (ash (aref data (+ 1 pos)) 8) (aref data pos)))

(defun parse-ixf-integer (data pos)
  "Read a 4-byte integer."
  (logior (ash (aref data (+ pos 3)) 24)
          (ash (aref data (+ pos 2)) 16)
          (ash (aref data (+ pos 1)) 8)
          (aref data pos)))

(defun parse-ixf-string (data pos length)
  (map 'string #'code-char (subseq data pos (+ pos length))))
