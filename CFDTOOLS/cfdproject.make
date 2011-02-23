#
default: this.tools this.library this.modules

PRJINCDIR=$(PRJDIR)/include
PRJINCEXT=$(PRJINCDIR)
PRJLIBDIR=$(PRJDIR)/lib
PRJEXEDIR=$(PRJDIR)/Tools
CONFIGDIR=$(PRJDIR)/../config

  dir.opt=optim
dir.optim=optim
  dir.dbg=debug
dir.debug=debug
 dir.prof=profil
dir.gprof=profil
   dir.omp=openmp
dir.openmp=openmp

# default
dir.=$(dir.opt)

PRJOBJDIR:=Obj.$(dir.$(opt))

$(PRJOBJDIR):
	mkdir -p $(PRJOBJDIR)

