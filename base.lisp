(in-package "DWF")

(define-foreign-library libdwf
  (t (:default "libdwf")))

(use-foreign-library libdwf)

;; mimic dwf's basic defines out of paranoia
(defctype bool :boolean)
(defctype dbyte :unsigned-char)

;; and the more actual defs
(defmacro defdwffield ((name &optional (base :int))
		       (set-name &optional (set-base :int))
		       &body keywords)
  `(progn
     (defcenum (,name ,base) ,@keywords)
     (defbitfield (,set-name ,set-base)
       ,@(mapcar (lambda (k)
		   (etypecase k
		     (keyword k)
		     (list (list (first k) (ash 1 (second k))))))
		 keywords))))
		       
(defctype hdwf :int)
(defcenum enumfilter :all :explorer :discovery)
(defcenum device-id (:explorer 1) :discovery)
(defcenum device-version
  (:explorer-c 2)
  (:explorer-e 4)
  (:explorer-f 5)
  (:discovery-a 1)
  (:discovery-b 2) ; collision apparently intentional v_v
  (:discovery-c 3))
(defdwffield (trigger-source dbyte) (trigger-source-set)
  :none
  :pc
  :detector-analog-in
  :detector-digital-in
  :analog-in
  :digital-in
  :digital-out
  :analog-out-1
  :analog-out-2
  :analog-out-3
  :analog-out-4
  :external-1
  :external-2
  :external-3
  :external-4)
(defctype state dbyte)
(defdwffield (acquisition-mode) (acquisition-mode-set)
  :single :scan-shift :scan-screen :record)
(defdwffield (filter) (filter-set)
  :decimate :average :min-max)
;; the fact the set is also a byte is probably a bug on the API's part
(defdwffield (analog-io dbyte) (analog-io-set dbyte)
  (:enable 1) :voltage :current :power :temperature)
(defdwffield (trigger-type) (trigger-type-set)
  :edge :pulse :transition)
(defdwffield (trigger-condition) (trigger-condition-set)
  :rising-positive :falling-negative)
(defdwffield (trigger-length) (trigger-length-set)
  :less :timeout :more)
(defctype error-code :int)
(defdwffield (digital-in-clock-source) (digital-in-clock-source-set)
  :internal :external)
(defdwffield (digital-in-sample-mode) (digital-in-sample-mode)
  :simple :noise)
(defdwffield (digital-out-output) (digital-out-output-set)
  :push-pull :open-drain :open-source :three-state)
(defdwffield (digital-out-type) (digital-out-type-set)
  :pulse :custom :random)
(defdwffield (digital-out-idle) (digital-out-idle-set)
  :init :low :high :zet)

;; these two are defined manually since the lisp wrappers of
;;  all others depend on them, and they can't signal errors... probably
;; (disassembly shows they indeed cannot in my implementation)
(defcfun (dwf-c:get-last-error "FDwfGetLastError")
    bool (code (:pointer error-code)))
;; not sure if string works here, since it's being written into
(defcfun (dwf-c:get-last-error-msg "FDwfGetLastErrorMsg")
    bool (msg (:pointer :char)))

(defmacro defdwf (name &body args)
  (multiple-value-bind (all-names stripped out in)
    (loop for a in args
       collect (first a) into all-names
       if (= (length a) 3)
       collect (list (first a) `(:pointer ,(second a))) into stripped
       and collect (list (first a) (second a)) into out
       else collect a into stripped
       and collect (first a) into in
       finally (return (values all-names stripped out in)))
    (let ((internal-lisp-name
	   (intern (symbol-name name) "DWF-C"))
	  (cname (concatenate
		  'string "FDwf"
		  (translate-camelcase-name name :upper-initial-p t))))
      `(progn
	 (defcfun (,internal-lisp-name ,cname) bool ,@stripped)
	 (defun ,name ,in
	   (with-foreign-objects ,(mapcar (lambda (o)
					    (list (first o)
						  `',(second o))) out)
	     (unless (,internal-lisp-name ,@all-names)
	       (error-dwf ',name))
	     (values ,@(mapcar
			(lambda (o)
			  `(mem-ref ,(first o) ',(second o)))
			out))))))))

(defun get-last-error ()
  (with-foreign-object (code 'error-code)
    (dwf-c:get-last-error code)
    (cffi:mem-ref code 'error-code)))

(defun get-last-error-msg ()
  (with-foreign-pointer-as-string (msg 512)
    (dwf-c:get-last-error-msg msg)))

(define-condition dwf-error (error)
  ((caller :reader dwf-error-caller :initarg :caller)
   (code :reader dwf-error-code :initform (get-last-error))
   (message :reader dwf-error-message :initform (get-last-error-msg)))
  (:report (lambda (c s)
	     (format s "WaveForms error in ~a: ~a (code ~d)"
		     (dwf-error-caller c)
		     (dwf-error-message c)
		     (dwf-error-code c)))))

(defun error-dwf (name) ; different name for dumb export reasons
  (error 'dwf-error :caller name))

;;; back to actual definitions

(defcfun (dwf-c:get-version "FDwfGetVersion") bool
  (v (:pointer :char)))
(defun get-version ()
  (with-foreign-pointer-as-string (version 32)
    (unless (dwf-c:get-version version)
      (error-dwf 'get-version))))

;; ENUMERATION, DEVICE MANAGEMENT
(defdwf enum (filter enumfilter) (count :int :out))
(defcfun (dwf-c:enum-device-type "FDwfEnumDeviceType") bool
  (device :int) (device-id (:pointer device-id))
  (device-version (:pointer device-version)))
(defun enum-device-type (device)
  (with-foreign-objects ((device-id 'device-id)
			 (device-version 'device-version))
    (unless (dwf-c:enum-device-type device device-id device-version)
      (error-dwf 'enum-device-type))
    (let ((id (mem-ref device-id 'device-id))
	  (v (mem-ref device-version 'device-version)))
      ;; kludge around the conflicting enum values in device-version
      (values id (if (and (eq id :explorer) (eq v :discovery-b))
		     :explorer-c
		     v)))))
(defdwf enum-device-is-opened (device :int) (used-p bool :out))
(defcfun (dwf-c:enum-user-name "FDwfEnumUserName") bool
  (device :int) (username (:pointer :char)))
(defun enum-user-name (device)
  (with-foreign-pointer-as-string (username 32) ; 32 spec'd in API
    (unless (dwf-c:enum-user-name device username)
      (error-dwf 'enum-user-name))))
(defcfun (dwf-c:enum-device-name "FDwfEnumDeviceName") bool
  (device :int) (device-name (:pointer :char)))
(defun enum-device-name (device)
  (with-foreign-pointer-as-string (device-name 32)
    (unless (dwf-c:enum-device-name device device-name)
      (error-dwf 'enum-device-name))))
(defcfun (dwf-c:enum-sn "FDwfEnumSN") bool
  (device :int) (sn (:pointer :char)))
(defun enum-sn (device)
  (with-foreign-pointer-as-string (sn 32)
    (unless (dwf-c:enum-sn device sn)
      (error-dwf 'enum-sn))))

(defdwf device-open (device :int) (hdwf hdwf :out))
(defdwf device-close (hdwf hdwf))
(defdwf device-close-all)
(defdwf device-auto-configure-set (hdwf hdwf) (autoconfigure bool))
(defdwf device-trigger-info
  (hdwf hdwf)
  (triggers trigger-source-set :out))
