(in-package "DWF")

;; current understanding of connection semantics: calling DEVICES
;;  invalidates all existing instances of DEVICE.
;; this is based on how FDwfEnum seems to work.
;; e.g., (enum :all) => 1, (enum-sn 0) => whatever, *unplug device*
;; (enum-sn 0) => whatever, (enum :all) => 0, (enum-sn 0) => error
;; add a :FORCE to DEVICES, maybe?

(defclass device ()
  ((type :initarg :type :accessor device-type
	 :type (member :explorer :discovery))
   (version :initarg :version :accessor device-version)
   (serial :initarg :serial :accessor device-serial)
   (num :initarg :num :accessor device-num)
   (handle :initarg :handle :initform nil :accessor device-handle
	 :type (or integer null))))

(defmethod print-object ((device device) s)
  (print-unreadable-object (device s :type t)
    ;; the "version" also includes the type, so print that
    (format s "~a ~a" (device-version device) (device-serial device))))

(defun device-from-id (id)
  (multiple-value-bind (type version) (enum-device-type id)
    (make-instance 'device
		   :num id :type type :version version
		   :serial (enum-sn id))))

(defun device-connected-p (device)
  (not (null (device-handle device))))

(defun device-connect (device)
  (let ((handle (device-open (device-num device))))
    (setf (device-handle device) handle)
    ;; We don't use the library's "auto configuration" feature.
    ;; The library's organization seems to be that few functions
    ;;  actually communicate with the device:
    ;;  *Reset, *Configure, and *Status.
    ;; The rest just set parameters in the library, which are then
    ;;  actually sent only when *Configure is called.
    ;; auto configuration does the equivalent of calling *Configure
    ;;  after every call that sets something in this manner.
    ;; But we don't need that, since we're usually setting multiple
    ;;  parameters anyway.
    (device-auto-configure-set handle nil)
    (values)))

(defun device-disconnect (device)
  (cond ((device-handle device)
	 (device-close (device-handle device))
	 (setf (device-handle device) nil)
	 (values))
	(t (error "Device is not connected."))))

(defun devices ()
  (loop for i below (enum :all) collecting (device-from-id i)))

(defvar *device*)

(defmacro with-device (device &body body)
  (let ((dvar (gensym "DEVICE")))
    `(let* ((,dvar ,device)
	    (*device* ,dvar))
       (unwind-protect (progn (device-connect ,dvar) ,@body)
	 (device-disconnect ,dvar)))))
