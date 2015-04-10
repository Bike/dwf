(in-package "DWF")

;; I don't know why, but this needs to wrap a-i-status?
(defmacro without-fp-traps (&body body)
  #+(and sbcl (or x86 x86-64))
  `(sb-int:with-float-traps-masked (:invalid :divide-by-zero)
     ,@body)
  #-(and sbcl (or x86 x86-64))
  `(progn ,@body))

(defdwffield (acquisition-mode) (acquisition-mode-set)
  :single :scan-shift :scan-screen :record)
(defdwffield (filter) (filter-set)
  :decimate :average :min-max)

;;; Control
(defdwf analog-in-reset (hdwf hdwf))
(defdwf analog-in-configure (hdwf hdwf) (reconfigure bool) (start bool))
(defdwf analog-in-status (hdwf hdwf) (read bool) (state state :out))
;; the following return data only from the last a-i-status call
(defdwf analog-in-status-samples-left (hdwf hdwf) (samples :int :out))
(defdwf analog-in-status-samples-valid (hdwf hdwf) (samples :int :out))
(defdwf analog-in-status-index-write (hdwf hdwf) (samples :int :out))
(defdwf analog-in-status-auto-triggered (hdwf hdwf) (auto bool :out))
(defcfun (dwf-c:analog-in-status-data "FDwfAnalogInStatusData") bool
  (hdwf hdwf) (channel :int) (data (:pointer :double)) (length :int))
(defun analog-in-status-data (hdwf channel length)
  (let ((out (make-array length :element-type 'double-float)))
    (with-foreign-object (data :double length)
      (unless (dwf-c:analog-in-status-data hdwf channel data length)
	(error-dwf 'analog-in-status-data))
      (loop for i from 0 below length do
	   (setf (aref out i) (mem-aref data :double i))))
    out))
(defdwf analog-in-status-sample (hdwf hdwf) (channel :int)
	(sample :double :out))
(defdwf analog-in-status-record
  (hdwf hdwf)
  (available :double :out) (lost :double :out) (corrupt :double :out))
;; not sure if these depend on -status or -configure
(defdwf analog-in-record-length-set (hdwf hdwf) (length :double))
(defdwf analog-in-record-length-get (hdwf hdwf) (length :double :out))

;;; Configuration
(defdwf analog-in-frequency-info (hdwf hdwf)
	(min :double :out) (max :double :out))
(defdwf analog-in-frequency-set (hdwf hdwf) (freq :double))
(defdwf analog-in-frequency-get (hdwf hdwf) (freq :double :out))
(defdwf analog-in-bits-info (hdwf hdwf) (bits :int :out))
(defdwf analog-in-buffer-size-info (hdwf hdwf)
	(min :int :out) (max :int :out))
(defdwf analog-in-buffer-size-set (hdwf hdwf) (size :int))
(defdwf analog-in-buffer-size-get (hdwf hdwf) (size :int :out))
(defdwf analog-in-acquisition-mode-info
  (hdwf hdwf) (modes acquisition-mode-set))
(defdwf analog-in-acquisition-mode-set
  (hdwf hdwf) (mode acquisition-mode))
(defdwf analog-in-acquisition-mode-get
  (hdwf hdwf) (mode acquisition-mode :out))

;;; Channels
(defdwf analog-in-channel-count (hdwf hdwf) (channels :int :out))
(defdwf analog-in-channel-enable-set (hdwf hdwf)
	(channel :int) (enable bool))
(defdwf analog-in-channel-enable-get (hdwf hdwf)
	(channel :int) (enable bool :out))
(defdwf analog-in-channel-filter-info
  (hdwf hdwf) (filters filter-set :out))
(defdwf analog-in-channel-filter-set (hdwf hdwf) (channel :int)
	(filter filter))
(defdwf analog-in-channel-filter-get (hdwf hdwf) (channel :int)
	(filter filter :out))
;(defdwf analog-in-channel-range-info (hdwf hdwf)
;(defdwf analog-in-channel-range-steps
(defdwf analog-in-channel-range-set (hdwf hdwf) (channel :int)
	(range :double))
(defdwf analog-in-channel-range-get (hdwf hdwf) (channel :int)
	(range :double :out))
;(defdwf analog-in-channel-offset-info
(defdwf analog-in-channel-offset-set (hdwf hdwf) (channel :int)
	(offset :double))
(defdwf analog-in-channel-offset-get (hdwf hdwf) (channel :int)
	(offset :double :out))
