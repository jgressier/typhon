.SUFFIXES:

#
# architecture dependent and project variables

PRJDIR=..
CONFIGDIR=$(PRJDIR)/../config

include $(PRJDIR)/cfdproject.make    # default target
include $(CONFIGDIR)/arch.make

LIBNAME:=libcfdfileformat

this.f90modules= \
  internal/TYPHON_FMT.f90  \
  internal/XBIN_DATA.f90  \
  internal/XBIN_IO.f90  \
  cgns/CGNS_STRUCT.f90  \

this.f90= $(this.f90modules) \
  internal/typhonread_ustmesh.f90  \
  internal/typhonwrite_ustmesh.f90 \
  cgns/readcgns_sol.f90        \
  cgns/readcgns_ustboco.f90    \
  cgns/readcgns_ustconnect.f90 \
  cgns/readcgnsvtex.f90        \
  cgns/readcgnszone.f90        \
  cgns/writecgns_bocomesh.f90  \
  cgns/writecgns_ustmesh.f90   \
  cgns/writecgns_sol.f90       \

LIBDEPS=cfdfileformat cfdmesh cfdbase
LOCALLINKOPT=-L$(PRJLIBDIR) $(LIBDEPS:%=-l%)

this.f90exe= xbininfo.f90  \
             ty2dmesh.f90

this.tools: $(this.f90exe:%.f90=$(PRJEXEDIR)/%)

include $(CONFIGDIR)/rules.make
include $(CONFIGDIR)/libconf.make

this.library: $(PRJLIBDIR)/$(LIBNAME).$(LIBSTA)

include depends.make


