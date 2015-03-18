(progn ; for stupid ## thing
  (defpackage "DWF"
    (:use "CL" "CFFI")
    (:export "DWF-ERROR")
    ;; devices.lisp
    (:export "DEVICE" "DEVICE-TYPE" "DEVICE-VERSION" "DEVICE-SERIAL"
	     "DEVICE-HANDLE" "DEVICE-FROM-ID" "DEVICES"
	     "DEVICE-CONNECTED-P" "DEVICE-CONNECT" "DEVICE-DISCONNECT")
    ;; the C API (DWF-C for raw versions, so to speak)
    #1=(:export "GET-LAST-ERROR" "GET-LAST-ERROR-MSG" "GET-VERSION"
		"ENUM" "ENUM-DEVICE-TYPE" "ENUM-DEVICE-IS-OPENED"
		"ENUM-USER-NAME" "ENUM-DEVICE-NAME" "ENUM-SN"
		"DEVICE-OPEN" "DEVICE-CLOSE" "DEVICE-CLOSE-ALL"
		"DEVICE-AUTO-CONFIGURE-SET" "DEVICE-TRIGGER-INFO"
		"ANALOG-OUT-RESET" "ANALOG-OUT-CONFIGURE"
		"ANALOG-OUT-STATUS" "ANALOG-OUT-NODE-PLAY-STATUS"
		"ANALOG-OUT-NODE-PLAY-DATA"
		"ANALOG-OUT-COUNT"
		"ANALOG-OUT-NODE-DATA-SET"))
  (defpackage "DWF-C"
    (:use)
    #1#))
