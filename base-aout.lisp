(in-package "DWF")

(defdwffield (func dbyte) (func-set)
  :dc :sine :square :triangle :ramp-up :ramp-down :noise
  (:custom 30) (:play 31))
(defdwffield (analog-out-node) (analog-out-node-set)
  :carrier :fm :am)

;; Control
(defdwf analog-out-reset (hdwf hdwf) (channel :int))
(defdwf analog-out-configure
  (hdwf hdwf) (channel :int) (start bool))
(defdwf analog-out-status
  (hdwf hdwf) (channel :int) (state state :out))
(defdwf analog-out-node-play-status
  (hdwf hdwf) (channel :int) (node analog-out-node)
  (free :int :out) (lost :int :out) (corrupted :int :out))
(defcfun (dwf-c:analog-out-node-play-data "FDwfAnalogOutNodePlayData")
    bool
  (hdwf hdwf) (channel :int) (node analog-out-node)
  (data (:pointer :double)) (data-length :int))
(defun analog-out-node-play-data (hdwf channel node data)
  (with-foreign-object (cdata :double (length data))
    (loop for i from 0 below (length data)
       do (setf (mem-aref cdata :double i) (aref data i)))
    (unless (dwf-c:analog-out-node-play-data hdwf channel node
					 cdata (length data))
      (error-dwf 'analog-out-node-play-data))))

;; Configuration
(defdwf analog-out-count (hdwf hdwf) (channels :int :out))
(defdwf analog-out-master-set (hdwf hdwf) (channel :int) (master :int))
(defdwf analog-out-master-get
  (hdwf hdwf) (channel :int) (master :int :out))
(defdwf analog-out-node-info
  (hdwf hdwf) (channel :int) (info analog-out-node-set :out))
(defdwf analog-out-node-enable-set
  (hdwf hdwf) (channel :int) (node analog-out-node) (enable bool))
(defdwf analog-out-node-enable-get
  (hdwf hdwf) (channel :int) (node analog-out-node) (on-p bool :out))
(defdwf analog-out-node-function-info
  (hdwf hdwf) (channel :int) (node analog-out-node)
  (funcs func-set :out))
(defdwf analog-out-node-function-set
  (hdwf hdwf) (channel :int) (node analog-out-node) (func func))
(defdwf analog-out-node-function-get
  (hdwf hdwf) (channel :int) (node analog-out-node) (func func :out))
(defdwf analog-out-node-frequency-info
  (hdwf hdwf) (channel :int) (node analog-out-node)
  (min :double :out) (max :double :out))
(defdwf analog-out-node-frequency-set
  (hdwf hdwf) (channel :int) (node analog-out-node) (freq :double))
(defdwf analog-out-node-frequency-get
  (hdwf hdwf) (channel :int) (node analog-out-node) (freq :double :out))
(defdwf analog-out-node-amplitude-info
  (hdwf hdwf) (channel :int) (node analog-out-node)
  (min :double :out) (max :double :out))
(defdwf analog-out-node-amplitude-set
  (hdwf hdwf) (channel :int) (node analog-out-node) (amp :double))
(defdwf analog-out-node-amplitude-get
  (hdwf hdwf) (channel :int) (node analog-out-node) (amp :double :out))
(defdwf analog-out-node-offset-info
  (hdwf hdwf) (channel :int) (node analog-out-node)
  (min :double :out) (max :double :out))
(defdwf analog-out-node-offset-set
  (hdwf hdwf) (channel :int) (node analog-out-node) (off :double))
(defdwf analog-out-node-offset-get
  (hdwf hdwf) (channel :int) (node analog-out-node) (off :double :out))
(defdwf analog-out-node-symmetry-info
  (hdwf hdwf) (channel :int) (node analog-out-node)
  (min :double :out) (max :double :out))
(defdwf analog-out-node-symmetry-set
  (hdwf hdwf) (channel :int) (node analog-out-node) (sym :double))
(defdwf analog-out-node-symmetry-get
  (hdwf hdwf) (channel :int) (node analog-out-node) (sym :double :out))
(defdwf analog-out-node-phase-info
  (hdwf hdwf) (channel :int) (node analog-out-node)
  (min :double :out) (max :double :out))
(defdwf analog-out-node-phase-set
  (hdwf hdwf) (channel :int) (node analog-out-node) (phz :double))
(defdwf analog-out-node-phase-get
  (hdwf hdwf) (channel :int) (node analog-out-node) (phz :double :out))
(defdwf analog-out-node-data-info
  (hdwf hdwf) (channel :int) (node analog-out-node)
  ;; these are erroneously given as int, double in the docs
  (min :int :out) (max :int :out))
(defcfun (dwf-c:analog-out-node-data-set "FDwfAnalogOutNodeDataSet")
    bool
  (hdwf hdwf) (channel :int) (node analog-out-node)
  (data (:pointer :double)) (length :int))
(defun analog-out-node-data-set (hdwf channel node data)
  (with-foreign-object (cdata :double (length data))
    (loop for i from 0 below (length data)
       do (setf (mem-aref cdata :double i) (aref data i)))
    (unless (dwf-c:analog-out-node-data-set hdwf channel node
					cdata (length data))
      (error-dwf 'analog-out-node-data-set))))

;; States
(defdwf analog-out-trigger-source-info
  (hdwf hdwf) (channel :int) (info trigger-source-set :out))
(defdwf analog-out-trigger-source-set
  (hdwf hdwf) (channel :int) (trigsrc trigger-source))
(defdwf analog-out-trigger-source-get
  (hdwf hdwf) (channel :int) (trigsrc trigger-source :out))
(defdwf analog-out-run-info
  (hdwf hdwf) (channel :int) (min :double :out) (max :double :out))
(defdwf analog-out-run-set (hdwf hdwf) (channel :int) (run :double))
(defdwf analog-out-run-get (hdwf hdwf) (channel :int) (r :double :out))
;; note to self: linked to analog-out-status
(defdwf analog-out-run-status
  (hdwf hdwf) (channel :int) (run :double :out))
(defdwf analog-out-wait-info
  (hdwf hdwf) (channel :int) (min :double :out) (max :double :out))
(defdwf analog-out-wait-set (hdwf hdwf) (channel :int) (wait :double))
(defdwf analog-out-wait-get (hdwf hdwf) (channel :int) (w :double :out))
(defdwf analog-out-repeat-info
  (hdwf hdwf) (channel :int) (min :int :out) (max :int :out))
(defdwf analog-out-repeat-set (hdwf hdwf) (channel :int) (repeat :int))
(defdwf analog-out-repeat-get (hdwf hdwf) (channel :int) (r :int :out))
(defdwf analog-out-repeat-trigger-set
  (hdwf hdwf) (channel :int) (repeat-p bool))
(defdwf analog-out-repeat-trigger-get
  (hdwf hdwf) (channel :int) (repeat-p bool :out))
