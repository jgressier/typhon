####### Définition des catalogues

#PRJDIR  = $(HDIR)/TYPHON
PRJDIR  = .
PRJINCDIR  =$(PRJDIR)/../include
#PRJLIBDIR  = $(PRJDIR)/Lib
PRJLIBDIR  = Lib
#PRJEXT  = ../LIBEXT
PRJLIBEXT = -L../../CFDTOOLS/lib -lcfdfileformat -lcfdmesh -lcfdmodels -lcfdmath -lcfdbase
PRJINCEXT =../CFDTOOLS/include


