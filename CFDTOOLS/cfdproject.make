PRJINCDIR=$(PRJDIR)/include
PRJINCEXT=$(PRJINCDIR)
PRJLIBDIR=$(PRJDIR)/lib
CONFIGDIR=$(PRJDIR)/../config

  dir.opt=optim
dir.optim=optim
  dir.dbg=debug
dir.debug=debug
 dir.prof=profil
dir.gprof=profil

# default
dir.=$(dir.opt)

PRJOBJDIR:=Obj.$(dir.$(opt))

$(PRJOBJDIR):
	mkdir -p $(PRJOBJDIR)
