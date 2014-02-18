####### directories + compilation options
#

PRJINCDIR = $(PRJDIR)/../include
CONFIGDIR = $(PRJDIR)/../config

PRJINCEXT = ../CFDTOOLS/include

PRJLIBEXT = -L../../CFDTOOLS/Lib.$(PRGEXT).$(optext) -lcfdfileformat -lcfdmesh -lcfdmodels -lcfdmath -lcfdbase

