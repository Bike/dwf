(in-package "DWF")

(defun awg-set (channel node
		&key function frequency amplitude offset
		  symmetry phase (device *device*))
  (analog-out-node-enable-set
   (device-handle device) channel node t)
  (when function
    (analog-out-node-function-set
     (device-handle device) channel node function))
  (when frequency
    (analog-out-node-frequency-set
     (device-handle device) channel node frequency))
  (when amplitude
    (analog-out-node-amplitude-set
     (device-handle device) channel node amplitude))
  (when offset
    (analog-out-node-offset-set
     (device-handle device) channel node offset))
  (when symmetry
    (analog-out-node-symmetry-set
     (device-handle device) channel node symmetry))
  (when phase
    (analog-out-node-phase-set
     (device-handle device) channel node phase)))

(defun awg-custom (channel node song &key (device *device*))
  (analog-out-node-data-set (device-handle device) channel node song))

(defun awg-start (channel &key (device *device*))
  (analog-out-configure (device-handle device) channel t))
