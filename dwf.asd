(asdf:defsystem #:dwf
  :description "Interface to Digilent WaveForms software."
  :author "Bike <aeshtaer@gmail.com>"
  :license "WTFPL"
  :version "0.1"
  :depends-on (#:cffi)
  :components ((:file "package")
	       (:file "base" :depends-on ("package"))
	       (:file "base-aout" :depends-on ("base" "package"))
	       (:file "base-ain" :depends-on ("base" "package"))
	       (:file "awg" :depends-on ("base-aout" "devices" "package"))
	       (:file "devices" :depends-on ("base" "package"))))
