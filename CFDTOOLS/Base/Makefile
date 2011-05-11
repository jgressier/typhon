.SUFFIXES:

default: this.library this.modules
#
# architecture dependent and project variables

PRJDIR=..
CONFIGDIR=$(PRJDIR)/../config

include $(PRJDIR)/cfdproject.make
include $(CONFIGDIR)/arch.make
include $(CONFIGDIR)/rules.make

LIBNAME:=libcfdbase

this.library: $(PRJLIBDIR)/$(LIBNAME).$(LIBSTA)

this.f90modules= \
  CONNECTIVITY.f90 \
  ENDIAN.f90       \
  IO_UNIT.f90      \
  IOCFD.f90        \
  MESHPREC.f90     \
  PACKET.f90       \
  STRING.f90       \

this.f90= $(this.f90modules) \

this.tools:


include $(CONFIGDIR)/libconf.make

include depends.make


