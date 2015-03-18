Digilent Inc.'s [WaveForms](http://www.digilentinc.com/Products/Detail.cfm?Prod=WAVEFORMS) is a graphical application for interacting with their Electronics Explorer and Analog Discovery hardware, both of which are headless USB-connected devices providing integrated oscilloscopes, function generators, logic analyzers, etc. The graphical application is Windows-only, but there is a C-accessible API also called WaveForms. The API provides, near as I can tell, all features of the device available in the graphical application, but less conveniently.

The WaveForms SDK includes a very basic program called dwfcmd for calling API functions from the command line. I have found this mode of interaction inconvenient. As such, this library is supposed to make things a bit easier. I doubt it will ever be as featured as the graphical application but it should hopefully make things tolerable.

Features:

* C API through CFFI
* Slightly lispier API, with same functions but no in-out parameters, signalling errors, etc.

Planned features:
* Another even smoother API layer (hiding device handles, specifying multiple wave properties at once, etc)
* Graphical oscilloscope display
