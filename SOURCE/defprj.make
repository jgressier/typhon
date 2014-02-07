####### directories + compilation options
#

PRJINCDIR = $(PRJDIR)/../include
CONFIGDIR = $(PRJDIR)/../config
PRJLIBEXT = -L../../CFDTOOLS/Lib.$(PRGEXT).$(optext) -lcfdfileformat -lcfdmesh -lcfdmodels -lcfdmath -lcfdbase
PRJINCEXT = ../CFDTOOLS/include

