.SUFFIXES:

default: this.library this.modules
#
# architecture dependent and project variables

PRJDIR=..
CONFIGDIR=$(PRJDIR)/../config

include $(PRJDIR)/cfdproject.make
include $(CONFIGDIR)/arch.make
include $(CONFIGDIR)/rules.make

LIBNAME:=libcfdfileformat

this.library: $(PRJLIBDIR)/$(LIBNAME).$(LIBSTA)

this.f90modules= \
  cgns/CGNS_STRUCT.f90  \

this.f90= $(this.f90modules)   \
  cgns/readcgns_sol.f90        \
  cgns/readcgns_ustboco.f90    \
  cgns/readcgns_ustconnect.f90 \
  cgns/readcgnsvtex.f90        \
  cgns/readcgnszone.f90        \
  cgns/writecgns_bocomesh.f90  \
  cgns/writecgns_ustmesh.f90   \
  cgns/writecgns_sol.f90       \


include $(CONFIGDIR)/libconf.make

include depends.make


