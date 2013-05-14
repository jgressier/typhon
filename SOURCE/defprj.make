####### directories + compilation options

#PRJDIR    = $(HDIR)/TYPHON
PRJDIR    = .
PRJINCDIR = $(PRJDIR)/../include
#PRJLIBDIR = $(PRJDIR)/Lib
PRJLIBDIR = Lib
PRJLIBEXT = -L../../CFDTOOLS/lib -lcfdfileformat -lcfdmesh -lcfdmodels -lcfdmath -lcfdbase
PRJINCEXT = ../CFDTOOLS/include


