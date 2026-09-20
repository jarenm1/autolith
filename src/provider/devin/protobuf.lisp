(in-package #:autolith)

;;;; -- Protobuf Wire Codec --

;;; A minimal protobuf wire-format codec for the Devin (Codeium Cascade)
;;; provider. It implements only the scalar and length-delimited field types
;;; the Devin messages use, with no schema compiler, reflection, or generated
;;; code. Field numbers and wire types are supplied by the message builders in
;;; devin/wire.lisp.

(defparameter +protobuf-wire-varint+ 0
  "The protobuf wire type for varint fields.")
(defparameter +protobuf-wire-fixed64+ 1
  "The protobuf wire type for fixed 64-bit fields.")
(defparameter +protobuf-wire-length-delimited+ 2
  "The protobuf wire type for length-delimited fields.")
(defparameter +protobuf-wire-fixed32+ 5
  "The protobuf wire type for fixed 32-bit fields.")

(-> protobuf--writer () (vector (unsigned-byte 8)))
(defun protobuf--writer ()
  "Return a fresh adjustable octet buffer for one protobuf message."
  (make-array 64
              :element-type '(unsigned-byte 8)
              :adjustable t
              :fill-pointer 0))

(-> protobuf--write-octet ((vector (unsigned-byte 8)) integer) null)
(defun protobuf--write-octet (buffer value)
  "Append VALUE as one octet to BUFFER."
  (vector-push-extend (logand value #xFF) buffer)
  nil)

(-> protobuf--write-varint ((vector (unsigned-byte 8)) integer) null)
(defun protobuf--write-varint (buffer value)
  "Append VALUE as a base-128 varint to BUFFER."
  (let ((remaining value))
    (loop
      (when (< remaining #x80)
        (protobuf--write-octet buffer remaining)
        (return))
      (protobuf--write-octet buffer (logior #x80 (logand remaining #x7F)))
      (setf remaining (ash remaining -7))))
  nil)

(-> protobuf--write-tag ((vector (unsigned-byte 8)) integer integer) null)
(defun protobuf--write-tag (buffer field wire-type)
  "Append the tag for FIELD and WIRE-TYPE to BUFFER."
  (protobuf--write-varint buffer (logior (ash field 3) wire-type))
  nil)

(-> protobuf--write-length-delimited
    ((vector (unsigned-byte 8)) integer (vector (unsigned-byte 8)))
    null)
(defun protobuf--write-length-delimited (buffer field octets)
  "Append FIELD's length-delimited OCTETS to BUFFER."
  (protobuf--write-tag buffer field +protobuf-wire-length-delimited+)
  (protobuf--write-varint buffer (length octets))
  (loop for octet across octets
        do (protobuf--write-octet buffer octet))
  nil)

(-> protobuf--string-octets (string) (vector (unsigned-byte 8)))
(defun protobuf--string-octets (value)
  "Return VALUE's UTF-8 octets."
  (sb-ext:string-to-octets value :external-format ':utf-8))

(-> protobuf-write-uint64 ((vector (unsigned-byte 8)) integer integer) null)
(defun protobuf-write-uint64 (buffer field value)
  "Append integer FIELD VALUE to BUFFER."
  (protobuf--write-tag buffer field +protobuf-wire-varint+)
  (protobuf--write-varint buffer value)
  nil)

(-> protobuf-write-bool ((vector (unsigned-byte 8)) integer t) null)
(defun protobuf-write-bool (buffer field value)
  "Append boolean FIELD VALUE to BUFFER."
  (protobuf-write-uint64 buffer field (if value 1 0)))

(-> protobuf-write-string ((vector (unsigned-byte 8)) integer string) null)
(defun protobuf-write-string (buffer field value)
  "Append string FIELD VALUE to BUFFER."
  (protobuf--write-length-delimited buffer field (protobuf--string-octets value)))

(-> protobuf-write-bytes
    ((vector (unsigned-byte 8)) integer (vector (unsigned-byte 8)))
    null)
(defun protobuf-write-bytes (buffer field value)
  "Append byte FIELD VALUE to BUFFER."
  (protobuf--write-length-delimited buffer field value))

(-> protobuf-write-double ((vector (unsigned-byte 8)) integer real) null)
(defun protobuf-write-double (buffer field value)
  "Append little-endian IEEE-754 double FIELD VALUE to BUFFER."
  (protobuf--write-tag buffer field +protobuf-wire-fixed64+)
  (let ((bits (sb-kernel:double-float-bits (coerce value 'double-float))))
    (loop for shift from 0 below 64 by 8
          do (protobuf--write-octet buffer (ldb (byte 8 shift) bits))))
  nil)

(-> protobuf-write-message
    ((vector (unsigned-byte 8)) integer (vector (unsigned-byte 8)))
    null)
(defun protobuf-write-message (buffer field octets)
  "Append nested message FIELD OCTETS to BUFFER."
  (protobuf--write-length-delimited buffer field octets))


;;;; -- Reader --

(defstruct (protobuf-reader (:constructor protobuf-reader-create (octets)))
  "A cursor over one protobuf message's OCTETS."
  (octets nil :type (vector (unsigned-byte 8)))
  (position 0 :type fixnum))

(-> protobuf-reader-exhausted-p (protobuf-reader) boolean)
(defun protobuf-reader-exhausted-p (reader)
  "Return true when READER has consumed every octet."
  (>= (protobuf-reader-position reader)
      (length (protobuf-reader-octets reader))))

(-> protobuf-read-varint (protobuf-reader) integer)
(defun protobuf-read-varint (reader)
  "Read one base-128 varint from READER."
  (let ((octets (protobuf-reader-octets reader))
        (result 0)
        (shift 0))
    (loop
      (when (>= (protobuf-reader-position reader) (length octets))
        (error 'provider-protocol-error
               :message "A protobuf varint ran past the end of its message."))
      (let ((octet (aref octets (protobuf-reader-position reader))))
        (incf (protobuf-reader-position reader))
        (setf result (logior result (ash (logand octet #x7F) shift)))
        (when (zerop (logand octet #x80))
          (return result))
        (incf shift 7)
        (when (> shift 63)
          (error 'provider-protocol-error
                 :message "A protobuf varint exceeded 64 bits."))))))

(-> protobuf-read-tag (protobuf-reader) (values integer integer))
(defun protobuf-read-tag (reader)
  "Read one field tag from READER, returning its field number and wire type."
  (let ((tag (protobuf-read-varint reader)))
    (values (ash tag -3) (logand tag #x7))))

(-> protobuf-read-length-delimited (protobuf-reader) (vector (unsigned-byte 8)))
(defun protobuf-read-length-delimited (reader)
  "Read one length-delimited payload from READER."
  (let* ((length (protobuf-read-varint reader))
         (start (protobuf-reader-position reader))
         (end (+ start length))
         (octets (protobuf-reader-octets reader)))
    (when (> end (length octets))
      (error 'provider-protocol-error
             :message "A protobuf length-delimited field ran past its message."))
    (setf (protobuf-reader-position reader) end)
    (subseq octets start end)))

(-> protobuf-read-string (protobuf-reader) string)
(defun protobuf-read-string (reader)
  "Read one UTF-8 string from READER."
  (sb-ext:octets-to-string (protobuf-read-length-delimited reader)
                           :external-format ':utf-8))

(-> protobuf-read-bool (protobuf-reader) boolean)
(defun protobuf-read-bool (reader)
  "Read one boolean from READER."
  (not (zerop (protobuf-read-varint reader))))

(-> protobuf-read-double (protobuf-reader) double-float)
(defun protobuf-read-double (reader)
  "Read one little-endian IEEE-754 double from READER."
  (let ((octets (protobuf-reader-octets reader))
        (start (protobuf-reader-position reader)))
    (when (> (+ start 8) (length octets))
      (error 'provider-protocol-error
             :message "A protobuf fixed64 field ran past its message."))
    (setf (protobuf-reader-position reader) (+ start 8))
    (let ((bits 0))
      (loop for index from 0 below 8
            do (setf bits (logior bits (ash (aref octets (+ start index))
                                            (* 8 index)))))
      (sb-kernel:make-double-float (ldb (byte 32 32) bits)
                                   (ldb (byte 32 0) bits)))))

(-> protobuf-skip-field (protobuf-reader integer) null)
(defun protobuf-skip-field (reader wire-type)
  "Skip one field of WIRE-TYPE from READER."
  (cond
    ((= wire-type +protobuf-wire-varint+)
     (protobuf-read-varint reader))
    ((= wire-type +protobuf-wire-fixed64+)
     (incf (protobuf-reader-position reader) 8))
    ((= wire-type +protobuf-wire-length-delimited+)
     (protobuf-read-length-delimited reader))
    ((= wire-type +protobuf-wire-fixed32+)
     (incf (protobuf-reader-position reader) 4))
    (t
     (error 'provider-protocol-error
            :message (format nil "Unknown protobuf wire type ~D." wire-type))))
  nil)
