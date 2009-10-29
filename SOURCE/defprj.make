####### Définition des catalogues

INCDIR  = $(HDIR)/LIB/Include
LIBDIR  = $(HDIR)/LIB/Lib
#PRJDIR  = $(HDIR)/TYPHON
PRJDIR  = .
PRJINCDIR  =$(PRJDIR)/Include
#PRJLIBDIR  = $(PRJDIR)/Lib
PRJLIBDIR  = Lib
#PRJEXT  = ../LIBEXT
PRJLIBEXT = -L../../CFDTOOLS/lib -lcfdmesh -lcfdbase
PRJINCEXT =../CFDTOOLS/include

  dir.opt=optim
dir.optim=optim
  dir.dbg=debug
dir.debug=debug
 dir.prof=profil
dir.gprof=profil

# default
dir.=$(dir.opt)

PRJOBJDIR=Obj.$(dir.$(opt))

$(PRJOBJDIR):
	mkdir -p $(PRJOBJDIR)
